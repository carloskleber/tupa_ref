"""Entry point: `tupa-gui [study.json]`."""

from __future__ import annotations

import argparse
import os
import sys

from PySide6.QtGui import QSurfaceFormat
from PySide6.QtWidgets import QApplication

from tupa_gui.view.main_window import MainWindow


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="tupa-gui", description=__doc__)
    parser.add_argument("study", nargs="?", help="input study JSON to open on launch")
    args = parser.parse_args(argv)

    # The Qt3D geometry view (view/viewer3d.py) embeds a Qt3DWindow via
    # QWidget.createWindowContainer, which is unreliable under Qt's native
    # "wayland" QPA platform (the view can render nothing, silently, while
    # the rest of the UI works fine). XWayland (the "xcb" platform) doesn't
    # have this problem. Prefer it whenever a Wayland session is detected,
    # unless the user already forced a platform explicitly.
    if "QT_QPA_PLATFORM" not in os.environ and os.environ.get("WAYLAND_DISPLAY"):
        os.environ["QT_QPA_PLATFORM"] = "xcb"

    # Must be set before QApplication exists (QWindow needs one already
    # running, so Qt3DWindow() itself can't be probed for its format here).
    # Mirrors what Qt3DWindow's own constructor requests; a window embedded
    # via createWindowContainer can otherwise inherit a depth-buffer-less
    # default format and render nothing — silently, no exception, no log.
    surface_format = QSurfaceFormat()
    surface_format.setDepthBufferSize(24)
    surface_format.setSamples(4)
    QSurfaceFormat.setDefaultFormat(surface_format)

    app = QApplication(sys.argv[:1])
    window = MainWindow()
    if args.study:
        window.open_study(args.study)
    window.show()
    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())
