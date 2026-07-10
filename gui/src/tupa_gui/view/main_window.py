"""Main window: tree view (G0), Qt3D geometry view (G1), results plots (G2)."""

from __future__ import annotations

from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QFileDialog, QMainWindow, QMessageBox, QSplitter, QTreeView

from tupa_gui.data import Results, ResultsLoadError, Study, StudyLoadError, load_results, load_study

from .plot_panel import PlotPanel
from .tree import build_study_model
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

        splitter = QSplitter(Qt.Orientation.Horizontal, self)
        splitter.addWidget(self._tree)
        splitter.addWidget(self._viewer)
        splitter.addWidget(self._plot_panel)
        splitter.setSizes([300, 650, 450])
        self.setCentralWidget(splitter)

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
        self._tree.setModel(build_study_model(study))
        self._tree.expandAll()
        self._viewer.load_study(study)

    def open_results(self, path: str | Path) -> None:
        try:
            results = load_results(path)
        except ResultsLoadError as exc:
            QMessageBox.critical(self, "Failed to load results", str(exc))
            return
        self.display_results(results)

    def display_results(self, results: Results) -> None:
        self._plot_panel.load_results(results)
