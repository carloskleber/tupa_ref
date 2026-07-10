"""Entry point: `tupa-gui [study.json]`."""

from __future__ import annotations

import argparse
import sys

from PySide6.QtWidgets import QApplication

from tupa_gui.view.main_window import MainWindow


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="tupa-gui", description=__doc__)
    parser.add_argument("study", nargs="?", help="input study JSON to open on launch")
    parser.add_argument("--results", help="results JSON (ADR 0012 schema) to open on launch")
    args = parser.parse_args(argv)

    app = QApplication(sys.argv[:1])
    window = MainWindow()
    if args.study:
        window.open_study(args.study)
    if args.results:
        window.open_results(args.results)
    window.show()
    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())
