"""Qt3D viewer for input geometry, authored elements only (G1, GUI_SDD.md §5.1a).

Renders exactly what the study JSON says (node positions, line endpoints) —
never the solver's per-segment discretisation (that is G4, and requires a
solver-side structure-dump export that does not exist yet).
"""

from __future__ import annotations

from PySide6.Qt3DCore import Qt3DCore
from PySide6.Qt3DExtras import Qt3DExtras
from PySide6.Qt3DRender import Qt3DRender
from PySide6.QtGui import QColor, QVector3D, QQuaternion
from PySide6.QtWidgets import QWidget

from tupa_gui.data import Study

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


class GeometryViewer(QWidget):
    """Embeds a Qt3DWindow showing a study's authored geometry."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._window = Qt3DExtras.Qt3DWindow()
        self._window.defaultFrameGraph().setClearColor(QColor(32, 34, 38))
        container = QWidget.createWindowContainer(self._window, self)
        # Keep the embedded window from being laid out to zero size (e.g. a
        # fully collapsed splitter pane): a windowed 3D surface at 0x0 is a
        # state some platforms/drivers recover from poorly.
        container.setMinimumSize(200, 200)

        from PySide6.QtWidgets import QVBoxLayout

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(container)

        # Python references to every Qt3D object of the current scene — see
        # the ownership note in load_study.
        self._scene: list[object] = []

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

        positions = [_to_qt3d(n.position) for n in study.nodes]
        extent = max((v.length() for v in positions), default=1.0)
        extent = max(extent, 1.0)

        self._add_soil_plane(root, scene, extent)
        for node in study.nodes:
            self._add_node(root, scene, _to_qt3d(node.position))
        for element in study.elements:
            a = _to_qt3d(study.node(element.from_node).position)
            b = _to_qt3d(study.node(element.to_node).position)
            self._add_conductor(root, scene, a, b, element.radius)

        camera = self._window.camera()
        camera.lens().setPerspectiveProjection(45.0, 16.0 / 9.0, 0.01, extent * 100)
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
        controller.setLinearSpeed(extent * 20)
        controller.setLookSpeed(180)
        scene.append(controller)

        self._window.setRootEntity(root)
        # Swap only after the new root is installed; releasing the previous
        # scene's references lets GC destroy the old (now undisplayed) scene.
        self._scene = scene

    def _add_soil_plane(self, root: Qt3DCore.QEntity, scene: list[object], extent: float) -> None:
        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QPlaneMesh(entity)
        size = max(extent * 3, 5.0)
        mesh.setWidth(size)
        mesh.setHeight(size)
        material = Qt3DExtras.QPhongAlphaMaterial(entity)
        material.setDiffuse(SOIL_COLOR)
        material.setAlpha(SOIL_COLOR.alphaF())
        entity.addComponent(mesh)
        entity.addComponent(material)
        scene += [entity, mesh, material]

    def _add_node(self, root: Qt3DCore.QEntity, scene: list[object], position: QVector3D) -> None:
        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QSphereMesh(entity)
        mesh.setRadius(0.05)
        material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(NODE_COLOR)
        transform = Qt3DCore.QTransform(entity)
        transform.setTranslation(position)
        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        scene += [entity, mesh, material, transform]

    def _add_conductor(
        self, root: Qt3DCore.QEntity, scene: list[object], a: QVector3D, b: QVector3D, radius: float
    ) -> None:
        direction = b - a
        length = direction.length()
        if length == 0:
            return

        entity = Qt3DCore.QEntity(root)
        mesh = Qt3DExtras.QCylinderMesh(entity)
        mesh.setRadius(max(radius, length * 0.005))
        mesh.setLength(length)
        material = Qt3DExtras.QPhongMaterial(entity)
        material.setDiffuse(CONDUCTOR_COLOR)

        # QCylinderMesh runs along +Y; rotate it onto the segment direction.
        y_axis = QVector3D(0, 1, 0)
        rotation = QQuaternion.rotationTo(y_axis, direction.normalized())
        transform = Qt3DCore.QTransform(entity)
        transform.setRotation(rotation)
        transform.setTranslation((a + b) * 0.5)

        entity.addComponent(mesh)
        entity.addComponent(material)
        entity.addComponent(transform)
        scene += [entity, mesh, material, transform]
