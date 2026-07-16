"""Main window: tree view (G0), Qt3D geometry view (G1), results plots (G2)."""

from __future__ import annotations

from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QFileDialog, QMainWindow, QMessageBox, QSplitter, QTabWidget, QTreeView

from tupa_gui.data import (
    Results,
    ResultsLoadError,
    Study,
    StudyLoadError,
    TransientResults,
    is_transient_results_file,
    load_results,
    load_study,
    load_transient_results,
)

from .plot_panel import PlotPanel, TransientPlotPanel
from .tree import ENTITY_ROLE, build_study_model
from .viewer3d import GeometryViewer


class MainWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.setWindowTitle("TUPÃ GUI")
        self.resize(1400, 700)

        self._tree = QTreeView()
        self._tree.setHeaderHidden(False)
        self._viewer = GeometryViewer()
        self._plot_panel = PlotPanel()
        self._transient_plot_panel = TransientPlotPanel()
        self._results_tabs = QTabWidget()
        self._results_tabs.addTab(self._plot_panel, "Frequency-domain")
        self._results_tabs.addTab(self._transient_plot_panel, "Transient")
        # (kind, id) -> tree item, rebuilt on every display_study; lets a 3D
        # click select the matching tree row (selection sync is bidirectional).
        self._tree_entity_items: dict[tuple[str, str], object] = {}

        splitter = QSplitter(Qt.Orientation.Horizontal, self)
        splitter.addWidget(self._tree)
        splitter.addWidget(self._viewer)
        splitter.addWidget(self._results_tabs)
        splitter.setSizes([300, 650, 450])
        self.setCentralWidget(splitter)

        self._viewer.nodeClicked.connect(lambda node_id: self._select_tree_item("node", node_id))
        self._viewer.elementClicked.connect(lambda element_id: self._select_tree_item("element", element_id))

        self._build_menu()

    def _build_menu(self) -> None:
        file_menu = self.menuBar().addMenu("&File")
        open_study_action = file_menu.addAction("&Open study…")
        open_study_action.triggered.connect(self._open_study_dialog)
        open_results_action = file_menu.addAction("Open &results…")
        open_results_action.triggered.connect(self._open_results_dialog)

    def _open_study_dialog(self) -> None:
        path, _ = QFileDialog.getOpenFileName(self, "Open study JSON", "", "JSON files (*.json)")
        if path:
            self.open_study(path)

    def _open_results_dialog(self) -> None:
        path, _ = QFileDialog.getOpenFileName(self, "Open results JSON", "", "JSON files (*.json)")
        if path:
            self.open_results(path)

    def open_study(self, path: str | Path) -> None:
        try:
            study = load_study(path)
        except StudyLoadError as exc:
            QMessageBox.critical(self, "Failed to load study", str(exc))
            return
        self.display_study(study)

    def display_study(self, study: Study) -> None:
        model, entity_items = build_study_model(study)
        self._tree_entity_items = entity_items
        self._tree.setModel(model)
        self._tree.expandAll()
        self._tree.selectionModel().currentChanged.connect(self._on_tree_selection_changed)
        self._viewer.load_study(study)

    def _on_tree_selection_changed(self, current, _previous) -> None:
        entity = current.data(ENTITY_ROLE)
        if entity is None:
            return
        kind, entity_id = entity
        if kind == "node":
            self._viewer.highlight_node(entity_id)
        else:
            self._viewer.highlight_element(entity_id)

    def _select_tree_item(self, kind: str, entity_id: str) -> None:
        item = self._tree_entity_items.get((kind, entity_id))
        if item is None:
            return
        self._tree.setCurrentIndex(item.index())

    def open_results(self, path: str | Path) -> None:
        try:
            transient = is_transient_results_file(path)
            results = load_transient_results(path) if transient else load_results(path)
        except ResultsLoadError as exc:
            QMessageBox.critical(self, "Failed to load results", str(exc))
            return
        if transient:
            self.display_transient_results(results)
        else:
            self.display_results(results)

    def display_results(self, results: Results) -> None:
        self._plot_panel.load_results(results)
        self._results_tabs.setCurrentWidget(self._plot_panel)

    def display_transient_results(self, results: TransientResults) -> None:
        self._transient_plot_panel.load_results(results)
        self._results_tabs.setCurrentWidget(self._transient_plot_panel)
