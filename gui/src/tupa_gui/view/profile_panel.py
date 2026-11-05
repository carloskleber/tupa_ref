"""Profile plot along contiguous conductors (quasi-stationary/DC view).

For each contiguous chain of segments (`tupa_gui.data.profile`) three linked
plots against the arc length: node voltage, longitudinal current I_l per
segment (steps) and transversal (leakage) current I_t per segment (bars).
The frequency selector defaults to the first entry of the sweep, which is the
DC point when the study asked for `frequencies.dc`. Dumb like the other view
widgets: it renders the `Profile` objects it is given.
"""

from __future__ import annotations

import math

import pyqtgraph as pg
from PySide6.QtWidgets import QComboBox, QHBoxLayout, QLabel, QVBoxLayout, QWidget

from tupa_gui.data import Profile, Results, Study, build_profiles

_PEN = pg.mkPen(color=(90, 170, 255), width=2)


class ProfilePanel(QWidget):
    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._chain_selector = QComboBox()
        self._freq_selector = QComboBox()
        self._chain_selector.currentIndexChanged.connect(self._plot_current)
        self._freq_selector.currentIndexChanged.connect(self._rebuild)

        row = QHBoxLayout()
        row.addWidget(QLabel("Frequency:"))
        row.addWidget(self._freq_selector, 1)
        row.addWidget(QLabel("Chain:"))
        row.addWidget(self._chain_selector, 2)

        self._info = QLabel()
        self._info.setWordWrap(True)

        self._graphics = pg.GraphicsLayoutWidget()
        self._v_plot = self._make_plot(0, "Node voltage (V)")
        self._il_plot = self._make_plot(1, "Longitudinal I_l (A)")
        self._it_plot = self._make_plot(2, "Transversal I_t (A)")
        self._it_plot.setLabel("bottom", "Distance along chain (m)")
        self._il_plot.setXLink(self._v_plot)
        self._it_plot.setXLink(self._v_plot)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addLayout(row)
        layout.addWidget(self._info)
        layout.addWidget(self._graphics, 1)

        self._study: Study | None = None
        self._results: Results | None = None
        self._profiles: list[Profile] = []
        self._notes = ""

    def _make_plot(self, row: int, label: str) -> pg.PlotItem:
        plot = self._graphics.addPlot(row=row, col=0)
        plot.setLabel("left", label)
        plot.showGrid(x=True, y=True, alpha=0.3)
        plot.getAxis("left").enableAutoSIPrefix(False)
        return plot

    def load(self, study: Study | None, results: Results | None) -> None:
        """(Re)build from a study and a frequency-domain results file; either
        may be missing, in which case the panel is cleared."""
        self._study, self._results = study, results
        self._freq_selector.blockSignals(True)
        self._freq_selector.clear()
        if results is not None:
            self._freq_selector.addItems([f"{f:g} Hz" for f in results.frequencies])
        self._freq_selector.blockSignals(False)
        self._rebuild()

    def _rebuild(self) -> None:
        self._profiles = []
        skipped: set[str] = set()
        if self._study is not None and self._results is not None and self._results.frequencies:
            index = max(0, self._freq_selector.currentIndex())
            self._profiles, skipped = build_profiles(self._study, self._results, index)
        self._chain_selector.blockSignals(True)
        self._chain_selector.clear()
        for k, p in enumerate(self._profiles, 1):
            c = p.chain
            self._chain_selector.addItem(f"{k}: {c.nodes[0]} → {c.nodes[-1]} ({len(c.segments)} segments)")
        self._chain_selector.blockSignals(False)
        notes = []
        if skipped:
            notes.append("skipped element types: " + ", ".join(sorted(skipped)))
        if self._study is not None and self._results is not None and not self._profiles:
            notes.append("no chain of line/catenary segments with voltage, i1 and i2 in the results "
                         "(an `outputs` filter may have dropped them)")
        self._notes = "; ".join(notes)
        self._info.setText(self._notes)
        self._plot_current()

    def _plot_current(self) -> None:
        for p in (self._v_plot, self._il_plot, self._it_plot):
            p.clear()
        index = self._chain_selector.currentIndex()
        if index < 0 or index >= len(self._profiles):
            return
        prof = self._profiles[index]
        c = prof.chain
        # real parts: the quasi-static solve is real to ~1e-5; report the residual
        worst_im = max(abs(v.imag) for v in prof.voltage + prof.longitudinal + prof.transversal)
        scale = max(abs(v.real) for v in prof.voltage + prof.longitudinal + prof.transversal) or 1.0
        text = f"f = {prof.frequency:g} Hz — Σ I_t = {prof.total_leakage.real:.6g} A"
        if worst_im > 1e-3 * scale:
            text += f" — imaginary parts reach {worst_im:.3g} (not a DC-like point; real parts plotted)"
        if self._notes:
            text += " — " + self._notes
        self._info.setText(text)

        v = [x.real for x in prof.voltage]
        if not all(math.isfinite(x) for x in v):
            self._info.setText(self._info.text() + " — non-finite samples in the results")
            return
        # an equipotential conductor spreads over ~1e-5 of its voltage: plot the
        # difference to the first node so the axis ticks stay readable
        ref = v[0] if (max(v) - min(v)) < 1e-3 * (abs(v[0]) or 1.0) else 0.0
        self._v_plot.setLabel("left", f"Node voltage − {ref:.6g} V (V)" if ref else "Node voltage (V)")
        v = [x - ref for x in v]
        self._v_plot.plot(c.s_nodes, v, pen=_PEN, symbol="o", symbolSize=5, symbolBrush=_PEN.color())
        il = [x.real for x in prof.longitudinal]
        self._il_plot.plot(c.s_nodes, il, pen=_PEN, stepMode="center")
        self._il_plot.plot(c.s_mid, il, pen=None, symbol="o", symbolSize=5, symbolBrush=_PEN.color())
        self._it_plot.addItem(pg.BarGraphItem(
            x=c.s_mid, height=[x.real for x in prof.transversal], width=c.lengths,
            brush=_PEN.color(), pen=pg.mkPen("k", width=0.5)))
