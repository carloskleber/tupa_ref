#!/usr/bin/env python3
"""Fault and lightning study of a coated pipeline near a transmission line
(ROADMAP Phase 14 item 2: the parts that run before the earth-return term of
item 1.3; report in docs/validation/pipeline-study.md).

One cross-section, generated here by `build_case` so that every variant differs
from the base by an explicit parameter:

  * an aerial conductor (shield-wire stand-in, ACSR Grosbeak equivalent) 30 m up
    along x from 0 to 1000 m, carried by one tower at x = 500 m;
  * the tower, a 0.3 m-radius vertical conductor down to the ground, then a
    0.5 m link to a four-arm star counterpoise (30 m arms, 0.5 m deep);
  * a DN 300 steel pipe (OD 323.9 mm, sigma 5e6 S/m, mu_r family) 1.5 m deep
    to its axis, 50 m from the line, 1 km long, with a 3 mm 3LPE coating whose
    conductivity follows the specific coating resistance r_c (sigma = t / r_c);
  * at each pipe end, either nothing (insulating joints, "isolated") or a 20 m
    bare 70 mm2 copper wire running away from the line ("earthed").

Two studies:

  fault      60 Hz harmonic, a phase-to-tower fault current injected at the
             tower top against remote earth; the aerial conductor is ended at
             both ends by a resistance standing for the rest of the shield-wire
             and tower ladder. This is the **conductive** coupling only: the
             inductive (earth-return) coupling from the faulted phase and the
             shield-wire return current is not in the model (ROADMAP item 1.3).
  lightning  a first stroke (Heidler, 31 kA, 1.8/95 us) to the tower top; the
             aerial conductor is matched at both ends (no reflections), NLT.

Coating stress: in the model the leakage current I_t = i1 + i2 of a coated
segment crosses the layer impedance z_c = ln(r_c/r0) / (2 pi (sigma_c + s eps_c) l)
in series with the soil path (theory.md §4.3), so the coating voltage is
I_t * z_c exactly. The fault study cross-checks it against the soil potential
computed at observation points on the coating surface; the lightning study
integrates the leakage current through the layer's G || C in the time domain.

Usage (from the repo root; needs the Fortran executable and numpy):

    python3 docs/validation/pipeline_checks.py --fortran-bin fortran/bin/Tupa \
        [--only fault,lightning,conv] [--out DIR] [--write-common]
"""
import argparse
import csv
import json
import math
import subprocess
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
EPS0 = 8.8541878128e-12

# Geometry (m)
H_AERIAL = 30.0
L = 1000.0
X_TOWER = 500.0
Y_PIPE = 50.0
Z_PIPE = -1.5
R_PIPE = 0.32385 / 2.0
T_COAT = 0.003
R_COAT = R_PIPE + T_COAT
EPSR_COAT = 2.3
N_PIPE = 500                      # 2 m segments (l >= 4 r, r_c < l/4)
SEG_PIPE = L / N_PIPE
ARM = 30.0
Z_STAR = -0.5
R_AERIAL = 0.012575
ZC_AERIAL = 60.0 * math.acosh(H_AERIAL / R_AERIAL)   # matched end, lightning

BASE = {
    "rc": 1.0e5,          # specific coating resistance, ohm m^2
    "mur": 300.0,         # pipe steel
    "ends": "earthed",    # or "isolated"
    "cross": True,        # numerics.crossMedium
    "rho": 1000.0,        # soil resistivity, ohm m
    "i_fault": 10.0e3,    # A at the tower top (fault study)
    "r_end": 10.0,        # ohm, shield-wire ladder beyond each end (fault study)
    "i_stroke": 31.0e3,   # A peak (lightning study)
}


def coat_sigma(rc):
    return T_COAT / rc


def build_case(study, p):
    """Return the case dict of `study` ("fault" or "lightning") for parameters p."""
    nodes = [
        {"id": "A0", "position": [0.0, 0.0, H_AERIAL]},
        {"id": "T_top", "position": [X_TOWER, 0.0, H_AERIAL]},
        {"id": "A1000", "position": [L, 0.0, H_AERIAL]},
        {"id": "T_gl", "position": [X_TOWER, 0.0, 0.0]},
        {"id": "F_hub", "position": [X_TOWER, 0.0, Z_STAR]},
        {"id": "F_xp", "position": [X_TOWER + ARM, 0.0, Z_STAR]},
        {"id": "F_xm", "position": [X_TOWER - ARM, 0.0, Z_STAR]},
        {"id": "F_yp", "position": [X_TOWER, ARM, Z_STAR]},
        {"id": "F_ym", "position": [X_TOWER, -ARM, Z_STAR]},
        {"id": "P0", "position": [0.0, Y_PIPE, Z_PIPE]},
        {"id": "P1000", "position": [L, Y_PIPE, Z_PIPE]},
    ]
    materials = [
        {"id": "acsr_grosbeak_eq", "epsilonr": 1.0, "mur": 1.0, "sigma": 22466019.4},
        {"id": "steel_tower", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.0e6},
        {"id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.8e7},
        {"id": "pipe_steel", "epsilonr": 1.0, "mur": p["mur"], "sigma": 5.0e6},
        {"id": "coat_3lpe", "epsilonr": EPSR_COAT, "mur": 1.0, "sigma": coat_sigma(p["rc"])},
    ]

    def line(id_, a, b, radius, segs, mat, **kw):
        e = {"type": "line", "id": id_, "from": a, "to": b, "radius": radius,
             "segments": segs, "material": mat}
        e.update(kw)
        return e

    elements = [
        line("AerialW", "A0", "T_top", R_AERIAL, 100, "acsr_grosbeak_eq"),
        line("AerialE", "T_top", "A1000", R_AERIAL, 100, "acsr_grosbeak_eq"),
        line("Tower", "T_top", "T_gl", 0.3, 15, "steel_tower"),
        line("Link", "T_gl", "F_hub", 0.00472, 1, "copper"),
        line("ArmXp", "F_hub", "F_xp", 0.00472, 15, "copper"),
        line("ArmXm", "F_hub", "F_xm", 0.00472, 15, "copper"),
        line("ArmYp", "F_hub", "F_yp", 0.00472, 15, "copper"),
        line("ArmYm", "F_hub", "F_ym", 0.00472, 15, "copper"),
        line("Pipe", "P0", "P1000", R_PIPE, N_PIPE, "pipe_steel",
             coating={"outerRadius": R_COAT, "material": "coat_3lpe"}),
    ]
    if p["ends"] == "earthed":
        nodes += [{"id": "E0", "position": [0.0, Y_PIPE + 20.0, -1.0]},
                  {"id": "E1000", "position": [L, Y_PIPE + 20.0, -1.0]}]
        elements += [line("Term0", "P0", "E0", 0.00472, 10, "copper"),
                     line("Term1000", "P1000", "E1000", 0.00472, 10, "copper")]
    elif p["ends"] != "isolated":
        raise ValueError(p["ends"])

    r_end = p["r_end"] if study == "fault" else round(ZC_AERIAL, 2)
    elements += [{"type": "branch", "id": "EndW", "from": "A0", "resistance": r_end},
                 {"type": "branch", "id": "EndE", "from": "A1000", "resistance": r_end}]

    case = {
        "title": "",
        "soil": {"conductivity": 1.0 / p["rho"], "permittivity": 10.0, "permeability": 1.0},
        "nodes": nodes,
        "materials": materials,
        "elements": elements,
        "numerics": {"crossMedium": "transmission"} if p["cross"] else {},
    }
    if not p["cross"]:
        del case["numerics"]

    desc = (f"DN 300 3LPE-coated steel pipe (r_c = {p['rc']:.0e} ohm m2, mu_r {p['mur']:g}) 1 km long, "
            f"1.5 m deep, 50 m from a 30 m high aerial conductor with one tower at x = 500 m on a "
            f"4 x 30 m star counterpoise; pipe ends {p['ends']}; rho = {p['rho']:g} ohm m; "
            f"air-soil coupling {'on' if p['cross'] else 'off'}")
    if study == "fault":
        case["title"] = (f"Tower-footing fault, conductive coupling only: {p['i_fault'] / 1e3:g} kA at "
                         f"50/60 Hz into the tower top, aerial conductor ended by {r_end:g} ohm; " + desc)
        case["sources"] = [{"node": "T_top", "current": {"re": p["i_fault"], "im": 0.0}}]
        case["frequencies"] = {"min": 50.0, "max": 60.0, "pointsPerDecade": 1}
        # soil potential on the coating surface, line side, every 20 m
        pts = [{"id": f"S{int(x)}", "position": [x, Y_PIPE - R_COAT, Z_PIPE]}
               for x in np.arange(0.0, L + 1.0, 20.0)]
        case["observation"] = {
            "points": pts,
            "touch": [{"id": "Touch_P0", "node": "P0", "radius": 1.0, "points": 16},
                      {"id": "Touch_mid", "node": "Pipe_n250", "radius": 1.0, "points": 16},
                      {"id": "Touch_P1000", "node": "P1000", "radius": 1.0, "points": 16}],
        }
    else:
        case["title"] = (f"First stroke {p['i_stroke'] / 1e3:g} kA (Heidler 1.8/95 us) to the tower top, "
                         f"aerial conductor matched ({r_end:g} ohm) at both ends; " + desc)
        obs_nodes = ["T_top", "F_hub", "P0", "Pipe_n125", "Pipe_n225", "Pipe_n250",
                     "Pipe_n275", "Pipe_n375", "P1000"]
        obs_el = [f"Pipe_e{k}" for k in range(5, N_PIPE + 1, 10)] + ["Pipe_e250", "Pipe_e251", "ArmYp_e15"]
        case["signal"] = {
            "waveform": "heidler",
            "imax": p["i_stroke"],
            "terms": [{"i0": p["i_stroke"], "n": 10.0, "tau1": 1.8e-6, "tau2": 95.0e-6}],
            "sourceNode": "T_top",
            "observeNodes": obs_nodes,
            "observeElectrodes": obs_el,
            "nyquistHz": 2.0e6,
            "fftPoints": p.get("fft", 1024),
            "transform": "nlt",
        }
    return case


# ---------------------------------------------------------------- running

def run(bin_, case, name, out):
    out.mkdir(parents=True, exist_ok=True)
    path = out / f"{name}.json"
    path.write_text(json.dumps(case, indent=2) + "\n")
    subprocess.run([str(bin_), "-q", "--output-dir", str(out), str(path)], check=True)
    return path


def cplx(v):
    return complex(v["re"], v["im"])


def z_coat_per_m(sig, s):
    """Coating impedance of one metre (ohm m) at complex frequency s."""
    return math.log(R_COAT / R_PIPE) / (2.0 * math.pi * (sig + s * EPSR_COAT * EPS0))


def pipe_x_of_node(nid):
    if nid == "P0":
        return 0.0
    if nid == "P1000":
        return L
    return int(nid.split("_n")[1]) * SEG_PIPE


# ---------------------------------------------------------------- fault

def dwight_star4(rho, arm, a, depth):
    """Dwight's four-point star (arm length `arm`, wire radius `a`, burial `depth`), ohm."""
    s = 2.0 * depth
    q = s / arm
    return rho / (8.0 * math.pi * arm) * (math.log(2.0 * arm / a) + math.log(2.0 * arm / s)
                                          + 2.912 - 1.071 * q + 0.645 * q ** 2 - 0.145 * q ** 4)


def analyse_fault(out, name, p, f=60.0):
    res = json.loads((out / f"{name}_results.json").read_text())
    k = min(range(len(res["frequencies"])), key=lambda i: abs(res["frequencies"][i] - f))
    vn = {n["id"]: cplx(n["voltage"][k]) for n in res["nodes"]}
    el = {e["id"]: (cplx(e["i1"][k]), cplx(e["i2"][k])) for e in res["electrodes"]}
    s = 2j * math.pi * res["frequencies"][k]
    zc = z_coat_per_m(coat_sigma(p["rc"]), s) / SEG_PIPE
    xs, vpipe, vcoat = [], [], []
    for j in range(1, N_PIPE + 1):
        i1, i2 = el[f"Pipe_e{j}"]
        xs.append((j - 0.5) * SEG_PIPE)
        vcoat.append((i1 + i2) * zc)
        a = vn["P0" if j == 1 else f"Pipe_n{j - 1}"]
        b = vn["P1000" if j == N_PIPE else f"Pipe_n{j}"]
        vpipe.append(0.5 * (a + b))
    xs, vpipe, vcoat = map(np.array, (xs, vpipe, vcoat))
    # observation cross-check: soil potential on the coating surface
    soil, touch = {}, {}
    with open(out / f"{name}_potentials.csv") as fh:
        for r in csv.DictReader(fh):
            if abs(float(r["frequency_hz"]) - res["frequencies"][k]) > 1e-6:
                continue
            v = complex(float(r["re"]), float(r["im"]))
            if r["quantity"] == "potential":
                soil[float(r["x"])] = v
            elif r["quantity"] == "touch":
                touch[r["id"]] = v
    chk = []
    for x, vs in soil.items():
        j = min(int(x / SEG_PIPE), N_PIPE - 1)
        chk.append((x, abs(vpipe[j] - vs), abs(vcoat[j])))
    jmax = int(np.argmax(abs(vcoat)))
    i_ends = 0.0
    if p["ends"] == "earthed":
        i_ends = sum(abs(sum(el[f"{t}_e{q}"][0] + el[f"{t}_e{q}"][1] for q in range(1, 11)))
                     for t in ("Term0", "Term1000"))
    i_foot = abs(sum(i1 + i2 for eid, (i1, i2) in el.items()
                     if eid.startswith(("Arm", "Link"))))
    return {
        "gpr": abs(vn["F_hub"]),
        "i_foot": i_foot,
        "r_foot": abs(vn["F_hub"]) / i_foot,
        "vpipe_max": float(abs(vpipe).max()),
        "vpipe_min": float(abs(vpipe).min()),
        "vpipe_P0": abs(vn["P0"]),
        "vpipe_mid": abs(vn["Pipe_n250"]),
        "vcoat_max": float(abs(vcoat)[jmax]),
        "x_vcoat_max": float(xs[jmax]),
        "vcoat_P0": float(abs(vcoat[0])),
        "soil_max": max(abs(v) for v in soil.values()),
        "chk": chk,
        "touch": {k_: abs(v) for k_, v in touch.items()},
        "i_ends": i_ends,
        "profile": (xs, vpipe, vcoat),
    }


def fault_section(bin_, out):
    variants = []
    for ends in ("isolated", "earthed"):
        for rc in (1e4, 1e5, 1e7):
            for mur in (100.0, 300.0, 1000.0):
                variants.append(dict(BASE, ends=ends, rc=rc, mur=mur))
    variants.append(dict(BASE, cross=False))
    rows = []
    for p in variants:
        name = f"pipe_fault_{p['ends']}_rc{p['rc']:.0e}_mur{int(p['mur'])}" + ("" if p["cross"] else "_nocross")
        run(bin_, build_case("fault", p), name, out)
        r = analyse_fault(out, name, p)
        rows.append((p, r))
        print(f"{name}: GPR {r['gpr'] / 1e3:.2f} kV (footing {r['i_foot'] / 1e3:.2f} kA, "
              f"{r['r_foot']:.1f} ohm), soil@pipe max {r['soil_max'] / 1e3:.2f} kV, "
              f"pipe {r['vpipe_min'] / 1e3:.3f}-{r['vpipe_max'] / 1e3:.3f} kV, "
              f"coating max {r['vcoat_max'] / 1e3:.2f} kV at x = {r['x_vcoat_max']:.0f} m, "
              f"touch {', '.join(f'{k}={v / 1e3:.2f} kV' for k, v in r['touch'].items())}", flush=True)
    base = next(r for p, r in rows if p == BASE)
    rd = dwight_star4(BASE["rho"], ARM, 0.00472, -Z_STAR)
    print(f"footing resistance: model {base['r_foot']:.2f} ohm at 60 Hz, Dwight four-point star "
          f"{rd:.2f} ohm ({(base['r_foot'] / rd - 1) * 100:+.1f} %)")
    err = max(abs(d - c) / max(c, 1.0) for _, d, c in base["chk"])
    worst = max(base["chk"], key=lambda t: abs(t[1] - t[2]) / max(t[2], 1.0))
    print(f"cross-check |V_pipe - psi(r_c)| vs |I_t z_c| (base): max rel. diff {err:.2e} "
          f"(x = {worst[0]:.0f} m: {worst[1]:.1f} V vs {worst[2]:.1f} V)")
    xs, vp, vc = base["profile"]
    np.savetxt(out / "pipe_fault_base_profile.csv",
               np.column_stack([xs, abs(vp), abs(vc)]), delimiter=",",
               header="x_m,abs_v_pipe_V,abs_v_coating_V", comments="", fmt="%.6g")
    return rows


# ---------------------------------------------------------------- lightning

def coat_voltage_time(t, it, sig):
    """Coating voltage of one segment from its leakage current: C dV/dt + G V = I_t (trapezoid)."""
    g = 2.0 * math.pi * sig * SEG_PIPE / math.log(R_COAT / R_PIPE)
    c = 2.0 * math.pi * EPSR_COAT * EPS0 * SEG_PIPE / math.log(R_COAT / R_PIPE)
    v = np.zeros_like(it)
    for n in range(len(t) - 1):
        dt = t[n + 1] - t[n]
        v[n + 1] = (v[n] * (c / dt - g / 2.0) + 0.5 * (it[n] + it[n + 1])) / (c / dt + g / 2.0)
    return v


def analyse_lightning(out, name, p):
    res = json.loads((out / f"{name}_transient_results.json").read_text())
    t = np.array(res["time"])
    vn = {n["id"]: np.array(n["voltage"]) for n in res["nodes"]}
    el = {e["id"]: np.array(e["i1"]) + np.array(e["i2"]) for e in res["electrodes"]}
    tmax = 100e-6       # NLT record end is unreliable (theory.md §8); judge the first 100 us
    m = t <= tmax
    vc = {}
    for k, it in el.items():
        if k.startswith("Pipe_e"):
            vc[k] = coat_voltage_time(t, it, coat_sigma(p["rc"]))
    kmax = max(vc, key=lambda k: abs(vc[k][m]).max())
    return {
        "v_top": abs(vn["T_top"][m]).max(),
        "gpr": abs(vn["F_hub"][m]).max(),
        "pipe": {k: abs(v[m]).max() for k, v in vn.items() if k.startswith("P")},
        "vcoat_max": abs(vc[kmax][m]).max(),
        "seg_vcoat_max": kmax,
        "t_vcoat_max": float(t[m][np.argmax(abs(vc[kmax][m]))]),
        "vcoat_250": abs(vc["Pipe_e250"][m]).max(),
        "vcoat_e5": abs(vc["Pipe_e5"][m]).max(),
        "series": (t, vn, vc),
    }


def lightning_section(bin_, out):
    variants = [BASE, dict(BASE, cross=False), dict(BASE, ends="isolated"),
                dict(BASE, rc=1e4), dict(BASE, rc=1e7)]
    rows = []
    for p in variants:
        name = (f"pipe_lightning_{p['ends']}_rc{p['rc']:.0e}_mur{int(p['mur'])}"
                + ("" if p["cross"] else "_nocross"))
        run(bin_, build_case("lightning", p), name, out)
        r = analyse_lightning(out, name, p)
        rows.append((p, r))
        print(f"{name}: tower top {r['v_top'] / 1e6:.2f} MV, footing {r['gpr'] / 1e3:.0f} kV, "
              f"pipe peaks " + ", ".join(f"{k}={v / 1e3:.2f} kV" for k, v in r["pipe"].items())
              + f"; coating max {r['vcoat_max'] / 1e3:.2f} kV ({r['seg_vcoat_max']}, "
              f"t = {r['t_vcoat_max'] * 1e6:.1f} us), at x=499 m {r['vcoat_250'] / 1e3:.2f} kV, "
              f"at x=9 m {r['vcoat_e5'] / 1e3:.3f} kV", flush=True)
    t, vn, vc = rows[0][1]["series"]
    np.savetxt(out / "pipe_lightning_base_series.csv",
               np.column_stack([t, vn["F_hub"], vn["Pipe_n250"], vn["P0"], vc["Pipe_e250"], vc["Pipe_e5"]]),
               delimiter=",", header="t_s,v_F_hub,v_Pipe_n250,v_P0,vcoat_e250,vcoat_e5",
               comments="", fmt="%.6g")
    return rows


def conv_section(bin_, out, base_rows=None):
    """Record-length check: the base lightning case on a 2048-sample record (512 us)."""
    names = {1024: "pipe_lightning_earthed_rc1e+05_mur300", 2048: "pipe_lightning_conv2048"}
    p2 = dict(BASE, fft=2048)
    if not (out / f"{names[1024]}_transient_results.json").exists():
        run(bin_, build_case("lightning", BASE), names[1024], out)
    run(bin_, build_case("lightning", p2), names[2048], out)
    r1 = analyse_lightning(out, names[1024], BASE)
    r2 = analyse_lightning(out, names[2048], p2)
    for key in ("v_top", "gpr", "vcoat_max", "vcoat_e5"):
        print(f"record 256 us vs 512 us, {key}: {r1[key] / 1e3:.2f} kV vs {r2[key] / 1e3:.2f} kV "
              f"({(r1[key] / r2[key] - 1) * 100:+.2f} %)")
    print(f"t of the coating peak: {r1['t_vcoat_max'] * 1e6:.1f} us vs {r2['t_vcoat_max'] * 1e6:.1f} us")


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--fortran-bin", default=str(ROOT / "fortran" / "bin" / "Tupa"))
    ap.add_argument("--only", default="fault,lightning")
    ap.add_argument("--out", default=str(ROOT / "fortran" / "results" / "pipeline"))
    ap.add_argument("--write-common", action="store_true",
                    help="regenerate common/pipeline_fault_60hz.json and common/pipeline_lightning.json")
    a = ap.parse_args()
    if a.write_common:
        for study, fname in (("fault", "pipeline_fault_60hz.json"), ("lightning", "pipeline_lightning.json")):
            (ROOT / "common" / fname).write_text(json.dumps(build_case(study, BASE), indent=2) + "\n")
            print(f"wrote common/{fname}")
    out = Path(a.out)
    only = a.only.split(",") if a.only else []
    if "fault" in only:
        fault_section(Path(a.fortran_bin), out)
    if "lightning" in only:
        lightning_section(Path(a.fortran_bin), out)
    if "conv" in only:
        conv_section(Path(a.fortran_bin), out)


if __name__ == "__main__":
    main()
