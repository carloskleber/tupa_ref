#!/usr/bin/env python3
"""Generate the Marx-generator laboratory case (docs/marx_generator.md).

A 2 MV, 10-stage Marx generator, a capacitive voltage divider, a tower
mockup with a 500 kV insulator string and a 50 m section of phase cable,
all inside a 40 x 40 x 40 m shielded hall (four wire-grid walls and a
ceiling; the floor is the soil half-space).

    python3 tools/gen_marx_lab.py > common/marx_lab_2MV.json
    python3 tools/gen_marx_lab.py --flashover > common/marx_lab_2MV_flashover.json
    python3 tools/gen_marx_lab.py --cell 10 --samples 256   # coarse smoke run
"""
import argparse
import json
import math

ap = argparse.ArgumentParser()
ap.add_argument("--stages", type=int, default=10)
ap.add_argument("--vstage", type=float, default=200e3, help="charging voltage per stage (V)")
ap.add_argument("--cstage", type=float, default=0.5e-6, help="stage capacitance (F)")
ap.add_argument("--rfront", type=float, default=40.0, help="front resistor per stage (ohm)")
ap.add_argument("--rgap", type=float, default=0.2, help="closed spark gap resistance per stage (ohm)")
ap.add_argument("--rtail", type=float, default=1400.0, help="tail (discharge) resistor at the output (ohm)")
ap.add_argument("--cell", type=float, default=5.0, help="enclosure grid cell (m)")
ap.add_argument("--room", type=float, default=40.0, help="cube edge (m)")
ap.add_argument("--wall-radius", type=float, default=None, help="wall-grid bar radius (m); default cell/(2 pi)")
ap.add_argument("--flashover", action="store_true", help="insulator string flashed over (arc) instead of open")
ap.add_argument("--cable-end", type=float, default=0.0, help="termination of the cable far end (ohm); 0 = open")
ap.add_argument("--no-room", action="store_true", help="omit the enclosure")
ap.add_argument("--samples", type=int, default=1024)
ap.add_argument("--nyquist", type=float, default=4e6)
a = ap.parse_args()

L = a.room
nodes, mats, els = [], [], []
node_ids = set()


pos = {}


def pt(i, x, y, z):
    """Declare node `i` at (x, y, z) and return its id."""
    if i not in node_ids:
        node_ids.add(i)
        pos[i] = (x, y, z)
        nodes.append({"id": i, "position": [round(x, 6), round(y, 6), round(z, 6)]})
    return i


def line(i, p, q, radius, mat, maxseg=2.5, lumped=None, segments=None):
    """Straight wire between node ids `p` and `q`."""
    length = math.dist(pos[p], pos[q])
    n = segments or max(1, math.ceil(length / maxseg))
    e = {"type": "line", "id": i, "from": p, "to": q, "radius": radius, "segments": n, "material": mat}
    if lumped:
        e["lumped"] = lumped
    els.append(e)


mats.append({"id": "steel", "epsilonr": 1.0, "mur": 1.0, "sigma": 1.0e7})
mats.append({"id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7})

# ---- Enclosure: four walls and a ceiling, wire grids on a `cell` pitch ----
zb = 0.5  # wall bottom: the floor is the ground plane itself
cells = max(1, round(L / a.cell))
rw = a.wall_radius or (L / cells) / (2 * math.pi)
if not a.no_room:
    # corner and mid-side earthing points (declared first so the planes reuse them)
    earth_pts = [(0, 0), (L / 2, 0), (L, 0), (L, L / 2), (L, L), (L / 2, L), (0, L), (0, L / 2)]
    for k, (x, y) in enumerate(earth_pts):
        pt(f"G{k}", x, y, zb)
        pt(f"R{k}", x, y, -3.0)
        line(f"Earth{k}", f"R{k}", f"G{k}", 0.015, "copper", segments=1)
    els.append({"type": "plane", "id": "WallS", "origin": [0, 0, zb], "u": [L, 0, 0], "v": [0, 0, L - zb],
                "cellsU": cells, "cellsV": cells, "radius": rw, "material": "steel"})
    els.append({"type": "plane", "id": "WallE", "origin": [L, 0, zb], "u": [0, L, 0], "v": [0, 0, L - zb],
                "cellsU": cells, "cellsV": cells, "radius": rw, "material": "steel"})
    els.append({"type": "plane", "id": "WallN", "origin": [L, L, zb], "u": [-L, 0, 0], "v": [0, 0, L - zb],
                "cellsU": cells, "cellsV": cells, "radius": rw, "material": "steel"})
    els.append({"type": "plane", "id": "WallW", "origin": [0, L, zb], "u": [0, -L, 0], "v": [0, 0, L - zb],
                "cellsU": cells, "cellsV": cells, "radius": rw, "material": "steel"})
    els.append({"type": "plane", "id": "Ceiling", "origin": [0, 0, L], "u": [L, 0, 0], "v": [0, L, 0],
                "cellsU": cells, "cellsV": cells, "radius": rw, "material": "steel"})

# ---- Marx column: stage k = (series branch: C, R_front + R_gap) then body wire ----
mx, my, z0, hs = 10.0, 28.0, 0.3, 0.7
r_stage = a.rfront + a.rgap
sources = []
pt("N0", mx, my, z0)
els.append({"type": "branch", "id": "MarxBase", "from": "N0", "resistance": 0.05})
for k in range(1, a.stages + 1):
    zlow = z0 + (k - 1) * hs
    pa = pt(f"A{k}", mx, my, zlow)
    els.append({"type": "branch", "id": f"Stage{k}", "from": f"A{k}", "to": f"N{k - 1}",
                "resistance": r_stage, "capacitance": a.cstage})
    pn = pt(f"N{k}", mx, my, zlow + hs)
    line(f"Body{k}", pa, pn, 0.1, "steel", segments=1)
    sources.append({"node": f"A{k}", "returnNode": f"N{k - 1}", "waveform": "branchNorton", "imax": a.vstage,
                    "resistance": r_stage, "inductance": 0.0, "capacitance": a.cstage})
ztop = z0 + a.stages * hs
top = f"N{a.stages}"
els.append({"type": "branch", "id": "TailR", "from": top, "resistance": a.rtail})

# ---- Voltage divider (damped capacitive): HV arm C1 + R_d embedded, LV arm C2 at the base ----
dx, dy = 14.0, 24.0
pt("Dtop", dx, dy, ztop)
pt("Dlv", dx, dy, 0.3)
line("DivLead", top, "Dtop", 0.01, "copper", maxseg=2.0)
line("Divider", "Dtop", "Dlv", 0.15, "steel", segments=7,
     lumped={"segment": 4, "resistance": 300.0, "capacitance": 600e-12})
els.append({"type": "branch", "id": "DivC2", "from": "Dlv", "capacitance": 300e-9, "resistance": 0.02})

# ---- Tower mockup: footing rod, mast, crossarm, 500 kV string, phase clamp P ----
tx, ty, th = 9.0, 38.0, 14.0
pt("TowerFoot", tx, ty, -2.5)
pt("TowerBase", tx, ty, 0.5)
pt("TowerTop", tx, ty, th)
line("Footing", "TowerFoot", "TowerBase", 0.02, "copper", segments=1)
line("Mast", "TowerBase", "TowerTop", 0.5, "steel", segments=3)
cx, cy = 3.0, 37.0
pt("Xarm", cx, cy, th)
pt("P", cx, cy, th - 4.0)
line("Crossarm", "TowerTop", "Xarm", 0.3, "steel", maxseg=3.0)
if a.flashover:
    string = {"segment": 1, "resistance": 1.0}   # arc channel across the string
else:
    string = {"segment": 1, "capacitance": 100e-12}  # string capacitance, no flashover
line("String", "Xarm", "P", 0.1, "steel", segments=2, lumped=string)

# ---- Lead from the generator to the phase clamp, and 50 m of phase cable ----
line("Lead", top, "P", 0.01, "copper", maxseg=2.5)
d = 50.0 / math.sqrt(2)
fx, fy = cx + d, cy - d
pt("F", fx, fy, th - 4.0)
line("Cable", "P", "F", 0.015, "copper", maxseg=2.5)
if a.cable_end > 0:
    els.append({"type": "branch", "id": "CableEnd", "from": "F", "resistance": a.cable_end})

case = {
    "title": "Marx generator 2 MV, divider, 500 kV tower mockup with 50 m phase cable, 40 m hall"
             + (" (insulator string flashed over)" if a.flashover else ""),
    "soil": {"conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0},
    "nodes": nodes,
    "materials": mats,
    "elements": els,
    "signal": {
        "sources": sources,
        "observeNodes": [top, "Dtop", "Dlv", "Xarm", "P", "F"],
        "nyquistHz": a.nyquist,
        "fftPoints": a.samples,
        "transform": "nlt",
    },
}
print(json.dumps(case, indent=1))
