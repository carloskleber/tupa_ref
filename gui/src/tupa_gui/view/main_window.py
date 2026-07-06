"""Main window: tree view (G0) + Qt3D geometry view (G1)."""

from __future__ import annotations

from pathlib import Path

from PySide6.QtWidgets import QFileDialog, QHBoxLayout, QMainWindow, QMessageBox, QTreeView, QWidget

from tupa_gui.data import Study, StudyLoadError, load_study

from .tree import build_study_model
from .viewer3d import GeometryViewer


class MainWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.setWindowTitle("TUPÃ GUI")
        self.resize(1100, 700)

        self._tree = QTreeView()
        self._tree.setHeaderHidden(False)
        self._tree.setMaximumWidth(360)
        self._viewer = GeometryViewer()

        # A QSplitter here (instead of a plain layout) leaves the embedded
        # Qt3DWindow's native window container rendering nothing on some
        # platforms — a known rough edge of createWindowContainer.
        central = QWidget(self)
        layout = QHBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self._tree)
        layout.addWidget(self._viewer, 1)
        self.setCentralWidget(central)

        self._build_menu()

    def _build_menu(self) -> None:
        file_menu = self.menuBar().addMenu("&File")
        open_action = file_menu.addAction("&Open study…")
        open_action.triggered.connect(self._open_study_dialog)

    def _open_study_dialog(self) -> None:
        path, _ = QFileDialog.getOpenFileName(self, "Open study JSON", "", "JSON files (*.json)")
        if path:
            self.open_study(path)

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
