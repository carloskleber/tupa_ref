"""PyQtGraph plot panel for solved results (G2, docs/GUI_SDD.md §7).

1D plots only: magnitude/phase vs. frequency for the input impedance, node
voltages, and electrode currents in a loaded `Results` (ADR 0012 schema).
Dumb like the other view widgets: given a `Results`, it renders — no JSON
parsing here.
"""

from __future__ import annotations

import cmath
import math

import pyqtgraph as pg
from PySide6.QtWidgets import QComboBox, QVBoxLayout, QWidget

from tupa_gui.data import Results


def _build_entries(results: Results) -> list[tuple[str, list[complex]]]:
    entries: list[tuple[str, list[complex]]] = []
    if results.input_impedance is not None:
        entries.append(("Input impedance Z_in", results.input_impedance))
    for n in results.nodes:
        entries.append((f"Voltage: {n.id}", n.voltage))
    for e in results.electrodes:
        entries.append((f"Longitudinal current i1: {e.id}", e.i1))
        entries.append((f"Transverse current i2: {e.id}", e.i2))
    return entries


class PlotPanel(QWidget):
    """A quantity selector plus linked magnitude/phase-vs-frequency plots."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._selector = QComboBox()
        self._selector.currentIndexChanged.connect(self._plot_current)

        self._graphics = pg.GraphicsLayoutWidget()
        self._magnitude_plot = self._graphics.addPlot(row=0, col=0)
        self._magnitude_plot.setLabel("left", "Magnitude")
        self._magnitude_plot.setLabel("bottom", "Frequency (Hz)")
        self._magnitude_plot.getAxis("bottom").enableAutoSIPrefix(False)
        self._magnitude_plot.setLogMode(x=True, y=False)
        self._magnitude_plot.showGrid(x=True, y=True, alpha=0.3)

        self._graphics.nextRow()
        self._phase_plot = self._graphics.addPlot(row=1, col=0)
        self._phase_plot.setLabel("left", "Phase (deg)")
        self._phase_plot.setLabel("bottom", "Frequency (Hz)")
        self._phase_plot.getAxis("bottom").enableAutoSIPrefix(False)
        self._phase_plot.setLogMode(x=True, y=False)
        self._phase_plot.showGrid(x=True, y=True, alpha=0.3)
        self._phase_plot.setXLink(self._magnitude_plot)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self._selector)
        layout.addWidget(self._graphics, 1)

        self._results: Results | None = None
        self._entries: list[tuple[str, list[complex]]] = []

    def load_results(self, results: Results) -> None:
        self._results = results
        self._entries = _build_entries(results)

        self._selector.blockSignals(True)
        self._selector.clear()
        self._selector.addItems([label for label, _ in self._entries])
        self._selector.blockSignals(False)

        self._plot_current()

    def clear(self) -> None:
        self._results = None
        self._entries = []
        self._selector.clear()
        self._magnitude_plot.clear()
        self._phase_plot.clear()

    def _plot_current(self) -> None:
        self._magnitude_plot.clear()
        self._phase_plot.clear()

        if self._results is None or not self._entries:
            return
        index = self._selector.currentIndex()
        if index < 0:
            return

        _, series = self._entries[index]
        freq = self._results.frequencies
        magnitude = [abs(v) for v in series]
        phase = [math.degrees(cmath.phase(v)) for v in series]

        pen = pg.mkPen(color=(90, 170, 255), width=2)
        self._magnitude_plot.plot(freq, magnitude, pen=pen, symbol="o", symbolSize=5, symbolBrush=pen.color())
        self._phase_plot.plot(freq, phase, pen=pen, symbol="o", symbolSize=5, symbolBrush=pen.color())
