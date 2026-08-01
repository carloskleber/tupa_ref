"""Qt3D viewer for input geometry, authored elements only (G1, GUI_SDD.md §5.1a).

Renders exactly what the study JSON says (node positions, line endpoints) —
never the solver's per-segment discretisation (that is G4, and requires a
solver-side structure-dump export that does not exist yet).
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from PySide6.Qt3DCore import Qt3DCore
from PySide6.Qt3DExtras import Qt3DExtras
from PySide6.Qt3DRender import Qt3DRender
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QColor, QQuaternion, QVector3D
from PySide6.QtWidgets import QLabel, QVBoxLayout, QWidget

from tupa_gui.data import LineElement, MeshElement, Study

# theory.md §2: right-handed, z up, air-soil interface at z = 0.
# Qt3D's convention is y-up; map study (x, y, z) -> Qt3D (x, z, y) so the
# study's z axis becomes Qt3D's "up" and the interface plane (Qt3D's
# default XZ plane, normal +Y) lands exactly at the study's z = 0.
def _to_qt3d(position: tuple[float, float, float]) -> QVector3D:
    x, y, z = position
    return QVector3D(x, z, y)


NODE_COLOR = QColor(255, 205, 60)
CONDUCTOR_COLOR = QColor(200, 120, 60)
SOIL_COLOR = QColor(110, 80, 55, 120)
HIGHLIGHT_COLOR = QColor(255, 60, 60)
GRID_COLOR = QColor(255, 255, 255, 45)
AXIS_COLOR_X = QColor(210, 70, 70)
AXIS_COLOR_Y = QColor(80, 190, 100)
AXIS_COLOR_Z = QColor(80, 140, 230)
INJECTION_COLOR = QColor(255, 40, 180)

# Conductor radius floor, as a fraction of the *scene's* overall extent —
# deliberately not a fraction of each conductor's own length. A study's
# authored radius (cm-scale) is routinely invisible at metre/decametre
# scene scale, so a floor is needed for conductors to render at all; but it
# must be the same floor for every conductor in the scene, or two elements
# authored with an identical radius end up drawn at different apparent
# thicknesses just because one is longer than the other.
MIN_VISIBLE_RADIUS_FRACTION = 0.0025


@dataclass
class _Highlightable:
    """One tree-selectable entity's Qt3D material(s). Usually a single
    material (one node sphere, one line's one cylinder); a `MeshElement`
    (ADR 0020) draws many bar cylinders for a single element ID, so all of
    them are held here together and highlighted/cleared as a unit."""

    materials: list[Qt3DExtras.QPhongMaterial]
    base_color: QColor


class GeometryViewer(QWidget):
    """Embeds a Qt3DWindow showing a study's authored geometry."""

    nodeClicked = Signal(str)
    elementClicked = Signal(str)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._window = Qt3DExtras.Qt3DWindow()
        self._window.defaultFrameGraph().setClearColor(QColor(32, 34, 38))
        self._window.renderSettings().pickingSettings().setPickMethod(
            Qt3DRender.QPickingSettings.PickMethod.TrianglePicking
        )
        container = QWidget.createWindowContainer(self._window, self)
        # Keep the embedded window from being laid out to zero size (e.g. a
        # fully collapsed splitter pane): a windowed 3D surface at 0x0 is a
        # state some platforms/drivers recover from poorly.
        container.setMinimumSize(200, 200)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(container)

        self._camera = self._window.camera()
        # Reposition node-id labels whenever the camera moves (orbit/pan/zoom
        # all change the view or projection matrix) or the pane is resized.
        self._camera.viewMatrixChanged.connect(self._update_label_positions)
        self._camera.projectionMatrixChanged.connect(self._update_label_positions)
        # A camera aspect ratio that doesn't match the actual viewport
        # stretches everything non-uniformly (spheres render as ellipsoids,
        # most visibly on studies whose nodes spread far from the view
        # centre, e.g. long spans). Re-derive it from the real window size
        # whenever that size changes, instead of assuming a fixed 16:9 pane.
        self._window.widthChanged.connect(self._update_aspect_ratio)
        self._window.heightChanged.connect(self._update_aspect_ratio)

        # Python references to every Qt3D object of the current scene — see
        # the ownership note in load_study.
        self._scene: list[object] = []
        self._node_entries: dict[str, _Highlightable] = {}
        self._element_entries: dict[str, _Highlightable] = {}
        self._node_positions: dict[str, QVector3D] = {}
        self._element_centers: dict[str, QVector3D] = {}
        # Node-id labels are plain QLabels overlaid on top of the Qt3D
        # window container and repositioned every frame the camera moves,
        # rather than Qt3D QText2DEntity billboards: on this Qt/driver combo
        # QText2DEntity's glyph atlas floods the log with RHI "failed to
        # upload buffers" every frame (reproduces with a minimal QText2DEntity
        # scene, unrelated to anything else in this view) — a 2D overlay
        # sidesteps that entirely and is the standard way to annotate a Qt3D
        # viewport anyway.
        self._node_labels: dict[str, QLabel] = {}
        self._highlighted: _Highlightable | None = None

    def load_study(self, study: Study) -> None:
        # PySide6 does not register Qt3D's QNode parent-child links as
        # ownership (QNode parenting is not the plain QObject parenting that
        # shiboken tracks), so a scene object whose Python wrapper becomes
        # unreferenced is destroyed — C++ side included — at the next Python
        # GC cycle. The scene then silently empties: the framegraph keeps
        # clearing the viewport, but there is nothing left to draw. Passing
        # `entity` as constructor parent is NOT enough. Every object created
        # here must therefore be appended to `scene` and kept alive on self
        # for as long as it is displayed.
        scene: list[object] = []
        node_entries: dict[str, _Highlightable] = {}
        element_entries: dict[str, _Highlightable] = {}
        node_positions: dict[str, QVector3D] = {}
        element_centers: dict[str, QVector3D] = {}

        self._clear_labels()

        root = Qt3DCore.QEntity()
        scene.append(root)

        light_entity = Qt3DCore.QEntity(root)
        light = Qt3DRender.QPointLight(light_entity)
        light.setColor(QColor(255, 255, 255))
        light.setIntensity(1.0)
        light_transform = Qt3DCore.QTransform(light_entity)
        light_transform.setTranslation(QVector3D(0, 20, 0))
        light_entity.addComponent(light)
        light_entity.addComponent(light_transform)
        scene += [light_entity, light, light_transform]

        for node in study.nodes:
            node_positions[node.id] = _to_qt3d(node.position)
        # A MeshElement (ADR 0020) plants its own main nodes rather than
        # referencing pre-declared ones — fold them into the same
        # id -> position map so they frame the scene, render as markers
        # (below) just like declared nodes, and resolve correctly if a
        # `line`/source references one of them by ID.
        for element in study.elements:
            if isinstance(element, MeshElement):
                for node_id, position in element.node_positions().items():
                    node_positions[node_id] = _to_qt3d(position)

        positions = list(node_positions.values())
        extent = max((v.length() for v in positions), default=1.0)
        extent = max(extent, 1.0)
        min_visible_radius = extent * MIN_VISIBLE_RADIUS_FRACTION
        # A fixed sphere radius (as the original code used) is a sub-pixel
        # dot once a study spans tens/hundreds of metres; scale with the
        # scene like the conductor floor above so node markers stay visible.
        node_radius = extent * 0.012

        self._add_soil_plane(root, scene, extent)
        self._add_grid(root, scene, extent)
        self._add_axes(root, scene, extent)
        for node_id, position in node_positions.items():
            entry = self._add_node(root, scene, position, node_id, node_radius)
            node_entries[node_id] = entry
            self._node_labels[node_id] = self._make_label(node_id)
        for element in study.elements:
            if isinstance(element, LineElement):
                a = node_positions[element.from_node]
                b = node_positions[element.to_node]
                entry = self._add_conductor(root, scene, a, b, element.radius, min_visible_radius, element.id)
                if entry is not None:
                    element_entries[element.id] = entry
                element_centers[element.id] = (a + b) * 0.5
            else:  # MeshElement (ADR 0020) — one cylinder per bar, all sharing the element's ID
                materials: list[Qt3DExtras.QPhongMaterial] = []
                centers: list[QVector3D] = []
                for from_id, to_id in element.bars():
                    a, b = node_positions[from_id], node_positions[to_id]
                    entry = self._add_conductor(root, scene, a, b, element.radius, min_visible_radius, element.id)
                    if entry is not None:
                        materials += entry.materials
                        centers.append((a + b) * 0.5)
                if materials:
                    element_entries[element.id] = _Highlightable(materials, CONDUCTOR_COLOR)
                if centers:
                    element_centers[element.id] = sum(centers, QVector3D()) * (1.0 / len(centers))
        self._add_injection_arrows(root, scene, study, node_positions, node_radius, extent)

        camera = self._camera
        camera.lens().setPerspectiveProjection(45.0, self._current_aspect_ratio(), 0.01, extent * 100)
        center = QVector3D(
            sum((p.x() for p in positions), 0.0) / len(positions) if positions else 0.0,
            sum((p.y() for p in positions), 0.0) / len(positions) if positions else 0.0,
            sum((p.z() for p in positions), 0.0) / len(positions) if positions else 0.0,
        )
        camera.setPosition(center + QVector3D(extent, extent, extent))
        camera.setUpVector(QVector3D(0, 1, 0))
        camera.setViewCenter(center)

        controller = Qt3DExtras.QOrbitCameraController(root)
        controller.setCamera(camera)
        # Qt3D's defaults (linearSpeed ~10) don't scale with scene size; the
        # previous extent*20 made pan/zoom fly across the whole scene on a
        # single wheel tick or drag for any study bigger than a few metres.
        # A gentler multiplier keeps a wheel tick/drag a small fraction of
        # the scene instead of blowing straight past it.
        controller.setLinearSpeed(max(extent * 1.5, 0.5))
        controller.setLookSpeed(180)
        scene.append(controller)

        self._window.setRootEntity(root)
        # Swap only after the new root is installed; releasing the previous
        # scene's references lets GC destroy the old (now undisplayed) scene.
        self._scene = scene
        self._node_entries = node_entries
        self._element_entries = element_entries
        self._node_positions = node_positions
        self._element_centers = element_centers
        self._highlighted = None
        self._update_label_positions()

    def highlight_node(self, node_id: str) -> None:
        """Highlight a node and re-centre orbiting on it (tree/3D selection sync)."""
        entry = self._node_entries.get(node_id)
        if entry is None:
            return
        self._set_highlight(entry)
        self._focus_camera(self._node_positions[node_id])

    def highlight_element(self, element_id: str) -> None:
        """Highlight an element and re-centre orbiting on it (tree/3D selection sync)."""
        entry = self._element_entries.get(element_id)
        if entry is None:
            return
        self._set_highlight(entry)
        self._focus_camera(self._element_centers[element_id])

    def clear_highlight(self) -> None:
        if self._highlighted is not None:
            for material in self._highlighted.materials:
                material.setDiffuse(self._highlighted.base_color)
            self._highlighted = None

    def _set_highlight(self, entry: _Highlightable) -> None:
        self.clear_highlight()
        for material in entry.materials:
            material.setDiffuse(HIGHLIGHT_COLOR)
        self._highlighted = entry

    def _focus_camera(self, target: QVector3D) -> None:
        # Re-centre the orbit controller on `target` while preserving the
        # current viewing distance/angle, so orbiting continues around
        # whatever was last selected instead of jumping to a fixed pose.
        camera = self._camera
        offset = camera.position() - camera.viewCenter()
        if offset.length() < 1e-6:
            offset = QVector3D(1.0, 1.0, 1.0)
        camera.setViewCenter(target)
        camera.setPosition(target + offset)

    def resizeEvent(self, event) -> None:  # noqa: N802 (Qt override)
        super().resizeEvent(event)
        self._update_label_positions()

    def _current_aspect_ratio(self) -> float:
        width, height = self._window.width(), self._window.height()
        return width / height if height > 0 else 16.0 / 9.0

    def _update_aspect_ratio(self) -> None:
        self._camera.lens().setAspectRatio(self._current_aspect_ratio())

    def _make_label(self, text: str) -> QLabel:
        label = QLabel(text, self)
        label.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents)
        label.setStyleSheet(
            "color: rgb(230, 230, 230); background-color: rgba(20, 20, 20, 150);"
            " padding: 0px 3px; border-radius: 2px; font-size: 11px;"
        )
        label.adjustSize()
        label.show()
        label.raise_()
        return label

    def _clear_labels(self) -> None:
        for label in self._node_labels.values():
            label.deleteLater()
        self._node_labels = {}

    def _update_label_positions(self) -> None:
        if not self._node_labels:
            return
        camera = self._camera
        view = camera.viewMatrix()
        view_projection = camera.projectionMatrix() * view
        width, height = self.width(), self.height()
        for node_id, label in self._node_labels.items():
            position = self._node_positions.get(node_id)
            if position is None:
                label.hide()
                continue
            # A point behind the camera still lands in [-1, 1] NDC after
            # QMatrix4x4.map's perspective divide (dividing by a negative w
            # flips the sign), so it must be culled using view-space depth
            # (Qt3D/OpenGL convention: camera looks down -Z, so z < 0 is in
            # front) rather than the mapped NDC coordinates alone.
            if view.map(position).z() >= 0:
                label.hide()
                continue
            # QMatrix4x4.map(QVector3D) applies the full projective
            # transform *and* the perspective divide, landing directly in
            # normalised device coordinates (each axis in [-1, 1] on screen).
            ndc = view_projection.map(position)
            if not (-1.2 <= ndc.x() <= 1.2 and -1.2 <= ndc.y() <= 1.2):
                label.hide()
                continue
            x = (ndc.x() * 0.5 + 0.5) * width
            y = (1.0 - (ndc.y() * 0.5 + 0.5)) * height
            label.show()
            label.move(int(x - label.width() / 2), int(y - label.height() - 6))

    def _add_soil_plane(self, root: Qt3DCore.QEntity, scene: list[object], extent: float) -> None:
        size = max(extent * 3, 5.0)
        # QPlaneMesh only has a front face (normal +Y); Qt3D's alpha-blended
        # materials cull the back face for correct blending order, so a
        # single plane vanishes once the camera orbits below study z = 0
        # (looking up at the interface from underground). A second copy
        # flipped 180 degrees about X gives the plane a visible backface
        # too, so the soil/air interface stays visible from either side.
        for flip in (False, True):
            entity = Qt3DCore.QEntity(root)
            mesh = Qt3DExtras.QPlaneMesh(entity)
            mesh.setWidth(size)
            mesh.setHeight(size)
            material = Qt3DExtras.QPhongAlphaMaterial(entity)
            material.setDiffuse(SOIL_COLOR)
            # Ambient too (see _add_plain_cylinder): the underside faces away
            # from the single overhead point light, so pure-diffuse shading
            # goes near-black there and the blend over the dark clear color
            # reads as "no plane at all" from below.
            material.setAmbient(SOIL_COLOR)
            material.setAlpha(SOIL_COLOR.alphaF())
            entity.addComponent(mesh)
            entity.addComponent(material)
            if flip:
                transform = Qt3DCore.QTransform(entity)
                transform.setRotation(QQuaternion.fromAxisAndAngle(QVector3D(1, 0, 0), 180.0))
                entity.addComponent(transform)
                scene.append(transform)
            scene += [entity, mesh, material]

    def _add_node(
        self,
        root: Qt3DCore.QEntity,
        scene: list[object],
        position: QVector3D,
        node_id: str,
        radius: float,
    ) -> _Highlightable:
        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QSphereMesh(entity)
        mesh.setRadius(radius)
        material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(NODE_COLOR)
        transform = Qt3DCore.QTransform(entity)
        transform.setTranslation(position)
        picker = Qt3DRender.QObjectPicker(entity)
        picker.clicked.connect(lambda _event, nid=node_id: self._on_node_picked(nid))
        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        entity.addComponent(picker)
        scene += [entity, mesh, material, transform, picker]
        return _Highlightable([material], NODE_COLOR)

    def _add_conductor(
        self,
        root: Qt3DCore.QEntity,
        scene: list[object],
        a: QVector3D,
        b: QVector3D,
        radius: float,
        min_visible_radius: float,
        element_id: str,
    ) -> _Highlightable | None:
        direction = b - a
        length = direction.length()
        if length == 0:
            return None

        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QCylinderMesh(entity)
        mesh.setRadius(max(radius, min_visible_radius))
        mesh.setLength(length)
        material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(CONDUCTOR_COLOR)

        transform = Qt3DCore.QTransform(entity)
        transform.setRotation(self._align_to_direction(direction))
        transform.setTranslation((a + b) * 0.5)

        picker = Qt3DRender.QObjectPicker(entity)
        picker.clicked.connect(lambda _event, eid=element_id: self._on_element_picked(eid))

        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        entity.addComponent(picker)
        scene += [entity, mesh, material, transform, picker]
        return _Highlightable([material], CONDUCTOR_COLOR)

    @staticmethod
    def _align_to_direction(direction: QVector3D) -> QQuaternion:
        """Rotation taking a mesh built along +Y onto `direction` (QCylinderMesh/QConeMesh convention)."""
        return QQuaternion.rotationTo(QVector3D(0, 1, 0), direction.normalized())

    def _add_plain_cylinder(
        self,
        root: Qt3DCore.QEntity,
        scene: list[object],
        a: QVector3D,
        b: QVector3D,
        radius: float,
        color: QColor,
    ) -> None:
        """A non-pickable, unlabelled cylinder segment — used for the grid and axes, which are
        scene decoration rather than authored geometry."""
        direction = b - a
        length = direction.length()
        if length == 0:
            return
        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QCylinderMesh(entity)
        mesh.setRadius(radius)
        mesh.setLength(length)
        if color.alpha() < 255:
            material = Qt3DExtras.QPhongAlphaMaterial(entity)
            material.setAlpha(color.alphaF())
        else:
            material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(color)
        # These thin markers are viewed edge-on as often as not (e.g. the
        # "up" axis, seen nearly end-on from the default isometric-ish
        # camera pose); a purely diffuse material goes near-black at grazing
        # incidence to the single point light. Ambient is an unconditional
        # term in Qt3D's phong shader, so setting it too keeps decoration
        # legible at any angle instead of only when well-lit.
        material.setAmbient(color)
        transform = Qt3DCore.QTransform(entity)
        transform.setRotation(self._align_to_direction(direction))
        transform.setTranslation((a + b) * 0.5)
        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        scene += [entity, mesh, material, transform]

    def _add_cone(
        self,
        root: Qt3DCore.QEntity,
        scene: list[object],
        apex: QVector3D,
        base: QVector3D,
        radius: float,
        color: QColor,
    ) -> None:
        """A cone from `base` (full `radius`) tapering to a point at `apex` — the arrowhead
        used by the axes and the injection-current marker."""
        direction = base - apex
        length = direction.length()
        if length == 0:
            return
        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QConeMesh(entity)
        mesh.setBottomRadius(0.0)
        mesh.setTopRadius(radius)
        mesh.setLength(length)
        material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(color)
        material.setAmbient(color)  # see _add_plain_cylinder: keeps arrowheads visible edge-on
        transform = Qt3DCore.QTransform(entity)
        transform.setRotation(self._align_to_direction(direction))
        transform.setTranslation((apex + base) * 0.5)
        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        scene += [entity, mesh, material, transform]

    @staticmethod
    def _grid_step(span: float, target_divisions: int = 20) -> float:
        """Smallest step from the 1-2-5 sequence giving at least `target_divisions` lines
        across `span` — the usual "nice round number" rule for a scale grid."""
        raw = span / target_divisions if target_divisions else span
        if raw <= 0:
            return 1.0
        magnitude = 10.0 ** math.floor(math.log10(raw))
        for mult in (1.0, 2.0, 5.0, 10.0):
            step = mult * magnitude
            if step >= raw:
                return step
        return 10.0 * magnitude

    def _add_grid(self, root: Qt3DCore.QEntity, scene: list[object], extent: float) -> None:
        """Reference grid on the soil plane (study z = 0), centred on the coordinate origin
        rather than the study's centroid, so it reads as an absolute scale/position reference."""
        half = max(extent * 1.5, 2.5)
        step = self._grid_step(half * 2)
        line_radius = extent * MIN_VISIBLE_RADIUS_FRACTION * 0.4
        count = max(int(half / step), 1)
        # Lines coplanar with the soil plane z-fight against it (both sit at
        # y = 0); lift the grid a hair above the surface to draw cleanly.
        y = extent * 0.001
        for i in range(-count, count + 1):
            offset = i * step
            self._add_plain_cylinder(
                root, scene, QVector3D(-half, y, offset), QVector3D(half, y, offset), line_radius, GRID_COLOR
            )
            self._add_plain_cylinder(
                root, scene, QVector3D(offset, y, -half), QVector3D(offset, y, half), line_radius, GRID_COLOR
            )

    def _add_axes(self, root: Qt3DCore.QEntity, scene: list[object], extent: float) -> None:
        """XYZ axes through the coordinate origin (study convention: x, y, z with z up),
        coloured by the usual red/green/blue = x/y/z convention regardless of which study
        axis Qt3D treats as "up" — gives the scene a zero reference independent of geometry."""
        length = max(extent * 0.6, 1.0)
        shaft_radius = extent * 0.003
        head_length = length * 0.12
        head_radius = shaft_radius * 3.0
        origin = QVector3D(0.0, 0.0, 0.0)
        axis_tips = (
            ((length, 0.0, 0.0), AXIS_COLOR_X),
            ((0.0, length, 0.0), AXIS_COLOR_Y),
            ((0.0, 0.0, length), AXIS_COLOR_Z),
        )
        for study_tip, color in axis_tips:
            tip = _to_qt3d(study_tip)
            shaft_end = tip * ((length - head_length) / length)
            self._add_plain_cylinder(root, scene, origin, shaft_end, shaft_radius, color)
            self._add_cone(root, scene, tip, shaft_end, head_radius, color)

    def _add_injection_arrows(
        self,
        root: Qt3DCore.QEntity,
        scene: list[object],
        study: Study,
        node_positions: dict[str, QVector3D],
        node_radius: float,
        extent: float,
    ) -> None:
        """A downward arrow touching each current-injection node (ADR 0010 sources, plus a
        transient study's signal source node), so the driven node(s) stand out from the rest."""
        injection_nodes = {source.node for source in study.sources}
        if study.signal is not None:
            injection_nodes.add(study.signal.source_node)

        up = QVector3D(0.0, 1.0, 0.0)
        arrow_length = max(extent * 0.18, node_radius * 6.0)
        head_length = arrow_length * 0.35
        shaft_radius = node_radius * 0.35
        head_radius = node_radius * 0.9
        for node_id in injection_nodes:
            tip = node_positions.get(node_id)
            if tip is None:
                continue
            head_base = tip + up * head_length
            shaft_end = tip + up * arrow_length
            self._add_plain_cylinder(root, scene, shaft_end, head_base, shaft_radius, INJECTION_COLOR)
            self._add_cone(root, scene, tip, head_base, head_radius, INJECTION_COLOR)

    def _on_node_picked(self, node_id: str) -> None:
        self.highlight_node(node_id)
        self.nodeClicked.emit(node_id)

    def _on_element_picked(self, element_id: str) -> None:
        self.highlight_element(element_id)
        self.elementClicked.emit(element_id)
