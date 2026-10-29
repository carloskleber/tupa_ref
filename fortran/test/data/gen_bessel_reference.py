# /// script
# requires-python = ">=3.10"
# dependencies = ["numpy", "scipy"]
# ///
"""Regenerate bessel_ratio_reference.csv: I0(z)/I1(z) from AMOS (scipy.special.ive,
the code SLATEC ZBESI is a copy of) on the domain the solver uses,
Re z >= 0, arg z in [0, pi/4] (ADR 0031).  Run: uv run gen_bessel_reference.py"""
import numpy as np
from pathlib import Path
from scipy.special import ive

radii = list(np.logspace(-6, np.log10(2000.0), 160))
radii += list(np.linspace(18.0, 32.0, 25))  # around the series/asymptotic switch
angles = np.radians([0.0, 1.0, 5.0, 15.0, 30.0, 40.0, 45.0])
rows = []
for a in angles:
    for r in radii:
        z = r * np.exp(1j * a)
        q = ive(0, z) / ive(1, z)
        rows.append((z.real, z.imag, q.real, q.imag))
out = Path(__file__).with_name("bessel_ratio_reference.csv")
np.savetxt(out, np.array(rows), delimiter=",", header="re_z,im_z,re_ratio,im_ratio",
           comments="", fmt="%.17e")
print(f"wrote {len(rows)} rows to {out}")
