#!/usr/bin/env python3
"""Generate the LabUAT impulse case: 32-stage Marx generator (GI), damped
capacitive divider (DIV), two 220 kV-class poles on three porticos and the
tower mockup, with an optional insulator-string arc.

Port of the legacy Matlab study `casos/cepel/labuat.{est,caso}` ("Estudo
oscilacoes LabUAT"). The geometry (nodes, bars, rings) is carried over
verbatim; the electrical pieces are rebuilt:

* GI: the legacy two-resistor Norton equivalent is replaced by the Marx model
  of docs/marx_generator.md, one `branch` per stage with its Norton source
  (`branchNorton`), stacked along the 24 m column. Data from the generator
  datasheet: 32 stages, front resistors 32 x 16 ohm + 33 x 12 ohm, tail
  resistors 64 x 130 ohm; stage capacitance 1 uF (two 62.5 nF legacy
  sections = 1 uF / 16 stages each), 187.5 kV per stage (6 MV).
* DIV: five 1.5 nF units with 120/120/120/200/200 ohm (bottom to top), the
  350 ohm external damping resistor in series at the top (CRO 6000-300
  datasheet); legacy values with `--divider legacy`. The legacy secondary arm
  (cdivsec) is kept.
* Toroids (`anel`) become `ring` elements, 10 chords each.

    python3 tools/gen_labuat.py > common/labuat_impulse.json
    python3 tools/gen_labuat.py --no-arc > common/labuat_impulse_noarc.json
    python3 tools/gen_labuat.py --stages 4 --samples 64    # smoke run

Assumptions not in the datasheet or the legacy files (flags in parentheses):
spark-gap resistance 0.2 ohm (--rgap), capacitor ESR 1 ohm taken out of the
front resistor (--esr), Marx column modelled as a 0.25 m-radius wire
(--body-radius), tail resistors two in parallel per stage, the intermediate
GI toroid tied to the middle of the column as in `labuat_gidiv.est`
(--no-int-toroid), overshoot compensation omitted, ideal ground images as in
the legacy `solo_ideal` flag (--soil-model).
"""
import argparse
import json
import math

ap = argparse.ArgumentParser()
ap.add_argument("--stages", type=int, default=32)
ap.add_argument("--vstage", type=float, default=187.5e3, help="charging voltage per stage (V)")
ap.add_argument("--cstage", type=float, default=1e-6, help="stage capacitance (F)")
ap.add_argument("--rf1", type=float, default=16.0, help="front resistor 1 per stage (ohm)")
ap.add_argument("--rf2", type=float, default=12.0, help="front resistor 2 per stage, and the extra one at the output (ohm)")
ap.add_argument("--rgap", type=float, default=0.2, help="closed spark gap resistance per stage (ohm)")
ap.add_argument("--esr", type=float, default=1.0, help="capacitor series resistance, taken out of the front resistor (ohm)")
ap.add_argument("--rtail", type=float, default=130.0, help="tail resistor value (ohm)")
ap.add_argument("--tail-per-stage", type=int, default=2, help="tail resistors per stage, in parallel")
ap.add_argument("--body-radius", type=float, default=0.25, help="Marx column wire radius (m)")
ap.add_argument("--divider", choices=["spec", "legacy"], default="spec")
ap.add_argument("--rext", type=float, default=350.0, help="external damping resistor of the divider, spec values only (ohm)")
ap.add_argument("--no-arc", action="store_true", help="omit the insulator-string arc (p1m - torreMe)")
ap.add_argument("--no-int-toroid", action="store_true", help="omit the intermediate GI toroid")
ap.add_argument("--soil-model", choices=["ideal", "frequency-dependent"], default="ideal")
ap.add_argument("--samples", type=int, default=1024)
ap.add_argument("--nyquist", type=float, default=5e6)
a = ap.parse_args()

# Legacy `Estrutura` material table (tools/legacy_import.py)
MATERIALS = {
    "a": ("aluminum", 1.0, 1.0, 3.54e7),
    "c": ("copper", 1.0, 1.0, 5.8e7),
    "s": ("steel", 1.0, 100.0, 5.88e6),
}

# ---- Legacy geometry (labuat.est), verbatim --------------------------------
# (id, x, y, z); the GI column nodes gim/gim2 are replaced by the Marx stages
NODES = [
    ("p1i", -110.0, -4.0, 30.0), ("p1conec", -30.0, -4.0, 30.0), ("p1m", 0.0, -4.0, 30.0), ("p1f", 110.0, -4.0, 30.0),
    ("divi", -30.69, -47.97, 0.0), ("divsec", -30.69, -47.97, 0.1), ("divint1", -30.69, -47.97, 4.5),
    ("divint2", -30.69, -47.97, 8.0), ("divint3", -30.69, -47.97, 11.0), ("divint4", -30.69, -47.97, 14.5),
    ("divf", -30.69, -47.97, 20.0), ("divt", -30.69, -47.97, -1.0), ("divt2", 50.69, -47.97, -1.0),
    ("gii", -40.69, -47.97, 0.0), ("gif", -40.69, -47.97, 24.0), ("git", -40.69, -47.97, -1.0),
    ("git2", -100.69, -47.97, -1.0),
    ("p2i", -110.0, 15.8, 30.0), ("p2m", 0.0, 15.8, 30.0), ("p2f", 110.0, 15.8, 30.0), ("p2at", 5.0, 15.8, 30.0),
    ("p2at2", 5.0, 15.8, 0.0), ("p2at3", 5.0, 15.8, -1.0),
    ("portSe", -110.0, -35.0, 0.0), ("portSd", -110.0, 35.0, 0.0), ("portSeT", -110.0, -35.0, 65.0),
    ("portSdT", -110.0, 35.0, 65.0), ("portSeV", -110.0, -30.0, 48.0), ("portSdV", -110.0, 30.0, 48.0),
    ("portSeF", -110.0, -35.0, -10.0), ("portSdF", -110.0, 35.0, -10.0),
    ("portCe", 0.0, -35.0, 0.0), ("portCd", 0.0, 35.0, 0.0), ("portCeT", 0.0, -35.0, 65.0),
    ("portCdT", 0.0, 35.0, 65.0), ("portCeV", 0.0, -30.0, 41.0), ("portCeV2", 0.0, -4.0, 41.0),
    ("portCdV", 0.0, 30.0, 41.0), ("portCdV2", 0.0, 16.0, 41.0), ("portCeF", 0.0, -35.0, -10.0),
    ("portCdF", 0.0, 35.0, -10.0),
    ("portNe", 110.0, -35.0, 0.0), ("portNd", 110.0, 35.0, 0.0), ("portNeT", 110.0, -35.0, 65.0),
    ("portNdT", 110.0, 35.0, 65.0), ("portNeV", 110.0, -30.0, 48.0), ("portNdV", 110.0, 30.0, 48.0),
    ("portNeF", 110.0, -35.0, -10.0), ("portNdF", 110.0, 35.0, -10.0),
    ("torreMd", 0.0, 15.8, 38.0), ("torreMc", 0.0, 5.9, 38.0), ("torreMe", 0.0, -4.0, 38.0),
    ("torreInf", 0.0, 5.9, 20.0),
]

# (id, centre, normal, ring radius, conductor radius, material)
RINGS = [
    ("yoke1a", (-110, -4, 30), (110, 0, 0), 3.0, 0.5, "s"),
    ("yoke1b", (110, -4, 30), (-110, 0, 0), 3.0, 0.5, "s"),
    ("yoke2a", (-110, 15.8, 30), (110, 0, 0), 3.0, 0.5, "s"),
    ("yoke2b", (110, 15.8, 30), (-110, 0, 0), 3.0, 0.5, "s"),
    ("torgi", (-40.69, -47.97, 24), (0, 0, 6), 3.75, 1.0, "a"),
    ("torgiint", (-40.69, -47.97, 11), (0, 0, 19), 2.3, 0.1, "a"),
    ("tordiv", (-30.69, -47.97, 20), (0, 0, 10), 3.0, 1.0, "a"),
    ("tordiv2", (-30.69, -47.97, 17.5), (0, 0, 12.5), 1.55, 0.4, "a"),
    ("tordiv3", (-30.69, -47.97, 14.5), (0, 0, 15.5), 0.8, 0.25, "a"),
    ("tordiv4", (-30.69, -47.97, 11), (0, 0, 19), 0.8, 0.25, "a"),
    ("tordiv5", (-30.69, -47.97, 8), (0, 0, 22), 0.4, 0.1, "a"),
    ("tordiv6", (-30.69, -47.97, 4.5), (0, 0, 25.5), 0.4, 0.1, "a"),
]
CHORDS = 10

# (id, from, to, radius, segments, material); the legacy repeats the ids p2n and p2at2
BARS = [
    ("p1s", "p1i", "p1conec", 0.56, 50, "a"), ("p1s2", "p1conec", "p1m", 0.56, 5, "a"),
    ("p1n", "p1m", "p1f", 0.56, 50, "a"), ("p2s", "p2i", "p2m", 0.56, 50, "a"),
    ("p2n", "p2m", "p2at", 0.56, 5, "a"), ("p2n2", "p2at", "p2f", 0.56, 50, "a"),
    ("atgi", "gii", "git", 0.01, 2, "c"), ("atdiv", "divi", "divt", 0.01, 2, "c"),
    ("bond1", "git", "git2", 0.01, 30, "c"), ("bond2", "divt", "divt2", 0.01, 30, "c"),
    ("bond3", "git", "divt", 0.01, 5, "c"), ("alim1", "gif", "divf", 0.001, 5, "c"),
    ("alim2", "divf", "p1conec", 0.00175, 20, "c"), ("ligtorgi", "gif", "torgi", 0.01, 1, "c"),
    ("ligtordiv", "divf", "tordiv", 0.01, 1, "c"), ("p2at1", "p2at", "p2at2", 0.002, 5, "c"),
    ("p2at2w", "p2at2", "p2at3", 0.01, 1, "c"), ("p2at4", "p2at3", "divt", 0.01, 15, "c"),
    ("conecy1a", "p1i", "yoke1a", 0.1, 1, "s"), ("conecy1b", "p1f", "yoke1b", 0.1, 1, "s"),
    ("conecy2a", "p2i", "yoke2a", 0.1, 1, "s"), ("conecy2b", "p2f", "yoke2b", 0.1, 1, "s"),
    ("conectordiv", "divf", "tordiv2", 0.1, 1, "s"), ("conectordiv1", "divint1", "tordiv6", 0.1, 1, "s"),
    ("conectordiv2", "divint2", "tordiv5", 0.1, 1, "s"), ("conectordiv3", "divint3", "tordiv4", 0.1, 1, "s"),
    ("conectordiv4", "divint4", "tordiv3", 0.1, 1, "s"),
    ("portColSe", "portSe", "portSeT", 0.5, 10, "s"), ("portColSd", "portSd", "portSdT", 0.5, 10, "s"),
    ("portColCe", "portCe", "portCeT", 0.5, 10, "s"), ("portColCd", "portCd", "portCdT", 0.5, 10, "s"),
    ("portColNe", "portNe", "portNeT", 0.5, 10, "s"), ("portColNd", "portNd", "portNdT", 0.5, 10, "s"),
    ("portTravS", "portSeT", "portSdT", 0.5, 10, "s"), ("portTravC", "portCeT", "portCdT", 0.5, 10, "s"),
    ("portTravN", "portNeT", "portNdT", 0.5, 10, "s"),
    ("portVigaS", "portSeV", "portSdV", 0.2, 10, "s"), ("portVigaC", "portCeV", "portCdV", 0.2, 10, "s"),
    ("portVigaN", "portNeV", "portNdV", 0.2, 10, "s"),
    ("portFundSe", "portSe", "portSeF", 0.2, 5, "s"), ("portFundSd", "portSd", "portSdF", 0.2, 5, "s"),
    ("portFundCe", "portCe", "portCeF", 0.2, 5, "s"), ("portFundCd", "portCd", "portCdF", 0.2, 5, "s"),
    ("portFundNe", "portNe", "portNeF", 0.2, 5, "s"), ("portFundNd", "portNd", "portNdF", 0.2, 5, "s"),
    ("torreMdw", "torreMd", "torreMc", 0.1, 5, "s"), ("torreMew", "torreMc", "torreMe", 0.1, 5, "s"),
    ("torre", "torreMc", "torreInf", 0.4, 20, "s"),
    ("torreAtd", "torreMd", "portCdV2", 0.02, 2, "s"), ("torreAte", "torreMe", "portCeV2", 0.02, 2, "s"),
    ("fitaCd", "portCdV", "portCd", 0.002, 10, "c"), ("fitaCe", "portCeV", "portCe", 0.002, 10, "c"),
    ("torreAtInf", "torreInf", "p2at2", 0.002, 5, "c"),
]
ARC = ("arco", "p1m", "torreMe", 0.1, 10, "c")  # legacy: active from t = 0

# Divider units, bottom (divsec) to top (divf): (id, from, to, R, C)
if a.divider == "spec":
    r_units = [120.0, 120.0, 120.0, 200.0, 200.0 + a.rext]
else:
    r_units = [100.0] * 5
DIV_TAPS = ["divsec", "divint1", "divint2", "divint3", "divint4", "divf"]
DIV_C = 1.5e-9

nodes, mats, els = [], {}, []


def node(i, x, y, z):
    nodes.append({"id": i, "position": [round(x, 6), round(y, 6), round(z, 6)]})


def material(letter):
    m = MATERIALS[letter]
    mats[m[0]] = {"id": m[0], "epsilonr": m[1], "mur": m[2], "sigma": m[3]}
    return m[0]


def bar(i, p, q, radius, segs, mat, lumped=None):
    e = {"type": "line", "id": i, "from": p, "to": q, "radius": radius, "segments": segs, "material": material(mat)}
    if lumped:
        e["lumped"] = lumped
    els.append(e)


def branch(i, p, q, r=0.0, l=0.0, c=0.0):
    e = {"type": "branch", "id": i, "from": p, "to": q}
    if r:
        e["resistance"] = r
    if l:
        e["inductance"] = l
    if c:
        e["capacitance"] = c
    els.append(e)


# ---- Passive structure ----------------------------------------------------
for n in NODES:
    node(*n)
for i, c, nrm, rr, rc, m in RINGS:
    if i == "torgiint" and a.no_int_toroid:
        continue
    els.append({"type": "ring", "id": i, "centre": list(c), "normal": list(nrm), "ringRadius": rr,
                "radius": rc, "chords": CHORDS, "material": material(m)})
for b in BARS:
    bar(*b)
if not a.no_arc:
    bar(*ARC)

# ---- Divider ---------------------------------------------------------------
for k in range(5):
    branch(f"cdiv{k + 1}", DIV_TAPS[k], DIV_TAPS[k + 1], r=r_units[k], c=DIV_C if a.divider == "spec" else 1500e-12)
branch("cdivsec", "divi", "divsec", r=1.655e5, c=0.4965e-6)  # secondary arm, legacy values

# ---- Marx generator ---------------------------------------------------------
n = a.stages
hs = 24.0 / n                      # column height 24 m: gii (z=0) to gif (z=24)
mx, my = -40.69, -47.97
r_front = a.rf1 + a.rf2 + a.rgap - a.esr   # series resistor of the stage, ESR taken out
r_tail = a.rtail / a.tail_per_stage
sources = []
top = "gii"
for k in range(1, n + 1):
    z = (k - 1) * hs
    prev = top
    node(f"A{k}", mx, my, z)       # upper side of the capacitor
    node(f"M{k}", mx, my, z)       # between the front resistor and the capacitor
    node(f"N{k}", mx, my, z + hs)  # top of the stage
    branch(f"Rf{k}", f"M{k}", prev, r=r_front)
    branch(f"C{k}", f"A{k}", f"M{k}", r=a.esr, c=a.cstage)
    branch(f"Rt{k}", f"A{k}", f"M{k}", r=r_tail)
    bar(f"Body{k}", f"A{k}", f"N{k}", a.body_radius, 1, "c")
    sources.append({"node": f"A{k}", "returnNode": f"M{k}", "waveform": "branchNorton", "imax": a.vstage,
                    "resistance": a.esr, "inductance": 0.0, "capacitance": a.cstage})
    top = f"N{k}"
branch("Rout", top, "gif", r=a.rf2)  # the 33rd front resistor
if not a.no_int_toroid:
    bar("ligtorgiint", f"N{n // 2}", "torgiint", 0.01, 1, "c")

case = {
    "title": "LabUAT impulse: 32-stage Marx generator, damped divider, porticos and tower mockup"
             + ("" if a.no_arc else " (insulator-string arc from t = 0)"),
    "soil": {"conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0},
    "numerics": {"imageModel": a.soil_model},
    "nodes": nodes,
    "materials": list(mats.values()),
    "elements": els,
    "signal": {
        "sources": sources,
        "observeNodes": ["gif", "divf", "divsec", "p1i", "p1f", "p2i", "p2f", "p1m", "torreMe"],
        "observeElectrodes": ["atgi_e1", "atdiv_e1"] + ([] if a.no_arc else ["arco_e1"]),
        "nyquistHz": a.nyquist,
        "fftPoints": a.samples,
        "transform": "nlt",
    },
}
print(json.dumps(case, indent=1))
