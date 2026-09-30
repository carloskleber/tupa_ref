#!/usr/bin/env python3
"""Convert a legacy TUPÃ Matlab case (`<name>.est` structure + `<name>.caso`
study) into a common/ JSON case (schema v1, common/README.md; ADR 0023).

    python3 tools/legacy_import.py <dir-with-legacy-cases> <name> [<name> ...] \
        [--out common] [--title "<English title>"]

Writes `<out>/<name>.json` per case and prints to stderr what could not be
carried over. The mapping follows the Matlab reader (`leentrada.m`,
`readCaso.m`, `lesinais.m`, `lecaso.m`), the model reference of record:

* `.est`: `no`/`non` nodes -> `nodes` (`N<label>`); `reta`/`haste`/`retan`/
  `hasten` -> `line`; `catenaria`/`catenarian` -> `catenary`; `cubo` (12 edges)
  and `piramide` (8 edges) -> one `line` per edge, since the Matlab element
  classes for them are empty placeholders. Every element is `E<k>`, k being the
  Matlab element number (the index its `il`/`it` outputs use). As in the Matlab,
  an element joining two already-connected nodes is skipped without taking a
  number, and a `cubo`/`piramide` edge whose segments would be shorter than
  `lmin` is re-segmented to ceil(length/lmin). Material letters map to the
  Matlab `Estrutura` material table.
* `.caso`: `solo`/`solo_freq` -> `soil` (legacy `kr`, at 1 rad/s, converted to
  the ADR 0007 form at 2π·1 MHz); the first `sinal` waveform -> `signal`
  (`portela`, or `rampa` as `portela` with alpha 0) at the legacy Nyquist
  frequency, FFT size and `freq_zero`; `func_tran` `u`/`deltau` nodes ->
  `observeNodes`, `il`/`it` elements -> `observeElectrodes` (first segment).
  A 1 A harmonic sweep at the injection node is added: `freq_log fmax n` keeps
  the legacy n log-spaced points over [fmax·1e-5, fmax]; `freq_lin fmax n`
  becomes 20 points/decade over [fmax·1e-4, fmax].
"""

import argparse
import json
import math
import sys
from pathlib import Path

# Matlab `Estrutura` material table: letter -> (id, epsilonr, mur, sigma)
MATERIALS = {
    "a": ("aluminum", 1.0, 1.0, 3.54e7),
    "c": ("copper", 1.0, 1.0, 5.8e7),
    "f": ("iron", 1.0, 1000.0, 1.0e7),
    "s": ("steel", 1.0, 100.0, 5.88e6),
    "z": ("zinc", 1.0, 1.0, 1.74e7),
    "w": ("copperweld", 1.0, 100.0, 2.7e6),
}

# Node-connection order of the Matlab lattice keywords (1-based vertex indices)
CUBE_EDGES = list(zip([1, 1, 2, 4, 5, 5, 6, 8, 1, 2, 3, 4], [2, 4, 3, 3, 6, 8, 7, 7, 5, 6, 7, 8]))
PYRAMID_EDGES = list(zip([1, 1, 2, 4, 1, 2, 3, 4], [2, 4, 3, 3, 5, 5, 5, 5]))

# Legacy element keywords this importer cannot represent yet
UNSUPPORTED = {
    "cabo", "cabon", "tubo", "tubon", "retanauto", "gruponos", "volume4", "tetra",
    "malha", "catenaria3", "anel", "helicoidal", "helicoidaln", "canal",
    "r", "rl", "rlc", "impedanciarlc", "z", "impedanciacomplexa",
}

OMEGA0 = 2.0 * math.pi * 1.0e6  # ADR 0007 reference frequency


def warn(msg):
    print(f"  - {msg}", file=sys.stderr)


def floats(line):
    return [float(x) for x in line.split()]


class Structure:
    def __init__(self):
        self.nodes = {}  # label -> (x, y, z), insertion-ordered
        self.elements = []
        self.materials = []
        self.linked = set()

    def node_id(self, label):
        if label not in self.nodes:
            raise ValueError(f"node '{label}' is not defined")
        return f"N{label}"

    def add_node(self, fields):
        label = fields[3] if len(fields) > 3 else str(len(self.nodes) + 1)
        self.nodes[label] = tuple(float(v) for v in fields[:3])

    def material(self, letter):
        name = MATERIALS[letter][0]
        if name not in self.materials:
            self.materials.append(name)
        return name

    def add(self, kind, a, b, radius, segments, letter, label=None, sag=None):
        """Add a two-node element unless a and b are already connected (Matlab `isLigado`)."""
        pair = frozenset((a, b))
        if pair in self.linked:
            return
        self.linked.add(pair)
        e = {
            "type": kind,
            "id": f"E{label or len(self.elements) + 1}",
            "from": self.node_id(a),
            "to": self.node_id(b),
        }
        if sag is not None:
            e["sag"] = sag
        e.update(radius=radius, segments=int(segments), material=self.material(letter))
        self.elements.append(e)

    def add_lattice(self, vertices, edges, radius, segments, lmin, letter):
        for i, j in edges:
            a, b = vertices[i - 1], vertices[j - 1]
            length = math.dist(self.nodes[a], self.nodes[b])
            n = int(segments)
            if length / n < lmin:
                n = math.ceil(length / lmin)
            self.add("line", a, b, radius, n, letter)


def parse_est(path):
    """Structure file, following `leentrada.m`: a line is a keyword or is skipped."""
    lines = path.read_text(errors="replace").splitlines()
    st = Structure()
    i = 0
    # The Matlab loop stops before the last line, which can only be data anyway
    while i < len(lines) - 1:
        key = lines[i].rstrip().lower()
        i += 1
        if key == "no":
            st.add_node(lines[i].split())
        elif key == "non":
            for _ in range(int(lines[i].split()[0])):
                i += 1
                st.add_node(lines[i].split())
        elif key in ("reta", "haste"):
            f = lines[i].split()
            st.add("line", f[0], f[1], float(f[2]), float(f[3]), f[4], f[5] if len(f) > 5 else None)
        elif key in ("retan", "hasten"):
            count, letter = lines[i].split()[:2]
            for _ in range(int(count)):
                i += 1
                f = lines[i].split()
                st.add("line", f[0], f[1], float(f[2]), float(f[3]), letter, f[4] if len(f) > 4 else None)
        elif key == "catenaria":
            f = lines[i].split()
            st.add("catenary", f[0], f[1], float(f[3]), float(f[4]), f[5], f[6] if len(f) > 6 else None,
                   sag=float(f[2]))
        elif key == "catenarian":
            count, letter = lines[i].split()[:2]
            for _ in range(int(count)):
                i += 1
                f = lines[i].split()
                st.add("catenary", f[0], f[1], float(f[3]), float(f[4]), letter,
                       f[5] if len(f) > 5 else None, sag=float(f[2]))
        elif key in ("cubo", "volume6"):
            f = lines[i].split()
            st.add_lattice(f[:8], CUBE_EDGES, float(f[8]), float(f[9]), float(f[10]), f[11])
        elif key in ("piramide", "volume5"):
            f = lines[i].split()
            st.add_lattice(f[:5], PYRAMID_EDGES, float(f[5]), float(f[6]), float(f[7]), f[8])
        elif key == "material":
            raise ValueError("case-specific 'material' definitions are not supported")
        elif key in UNSUPPORTED:
            raise ValueError(f"legacy element '{key}' has no JSON counterpart yet")
        else:
            i -= 1
        i += 1
    return st


def parse_signal(kind, line):
    """One `sinal` entry (`lesinais.m`); returns (node label, signal fields)."""
    f = line.split()
    if kind == "portela":  # node imax alpha tFront tTopEnd tTailEnd
        v = [float(x) for x in f[1:6]]
        return f[0], dict(waveform="portela", imax=v[0], alpha=v[1], tFront=v[2], tTopEnd=v[3], tTailEnd=v[4])
    if kind == "rampa":  # node imax tFront tTopEnd tTailEnd
        v = [float(x) for x in f[1:5]]
        return f[0], dict(waveform="portela", imax=v[0], alpha=0.0, tFront=v[1], tTopEnd=v[2], tTailEnd=v[3])
    raise ValueError(f"legacy signal '{kind}' has no JSON counterpart yet")


def parse_caso(path):
    lines = [ln.rstrip() for ln in path.read_text(errors="replace").splitlines()]
    c = dict(flags=set(), signals=[], nodes=[], elements=[], skipped=[], fft=1024, freq_zero=None)
    i = 0
    while i < len(lines):
        key = lines[i].lower()
        nxt = lines[i + 1] if i + 1 < len(lines) else ""
        if key == "nome":
            c["name"] = nxt.strip()
        elif key in ("solo", "ar", "solo_freq"):
            c[key] = floats(nxt)
            if key == "solo_freq":
                c["flags"].add(key)
        elif key == "num_pontos_fft":
            c["fft"] = int(nxt.split()[0])
        elif key == "freq_zero":
            c["freq_zero"] = float(nxt.split()[0])
        elif key in ("freq_log", "freq_lin", "freq_quad", "freq_div"):
            v = floats(nxt)
            c["freq"] = (key, v[0], int(v[1]))
        elif key == "sinal":
            # The count line is missing in some cases; the Matlab then reads none
            # (a latent bug) — the evident intent is a single signal.
            j = i + 1
            if nxt.strip().isdigit():
                count, j = int(nxt), i + 2
            else:
                count = 1
            for _ in range(count):
                c["signals"].append(parse_signal(lines[j].strip(), lines[j + 1]))
                j += 2
            i = j
            continue
        elif key == "func_tran":
            var = lines[i + 3].split()[0]
            data = lines[i + 4].split() if i + 4 < len(lines) else []
            title = lines[i + 1].strip()
            if var in ("u", "deltau"):
                c["nodes"] += [n for n in data if n not in c["nodes"]]
            elif var in ("il", "it"):
                c["elements"] += [e for e in data if e not in c["elements"]]
            else:
                c["skipped"].append(f"func_tran '{var}' ({title})")
            i += 5
            continue
        elif key in ("solo_ideal", "sem_mutua", "prop_inst", "toda_freq", "sinaldif"):
            c["flags"].add(key)
        i += 1
    return c


def soil_block(c):
    if "solo_freq" in c["flags"]:
        sigma0, alpha, kr, mur = c["solo_freq"]
        # Legacy W = s0 + kr[1 + j tan(pi a/2)] w^a (w0 = 1 rad/s) equals the
        # ADR 0007 form s0 + kr'[cot(pi a/2) + j](w/w0)^a with kr' = kr tan(pi a/2) w0^a.
        return {"type": "portela", "permeability": mur, "sigma0": sigma0, "alpha0": alpha,
                "kr": float(f"{kr * math.tan(math.pi * alpha / 2) * OMEGA0 ** alpha:.6g}")}
    sigma, epsr, mur = c["solo"]
    return {"conductivity": sigma, "permittivity": epsr, "permeability": mur}


def convert(folder, name, title):
    print(f"{name}:", file=sys.stderr)
    st = parse_est(folder / f"{name}.est")
    c = parse_caso(folder / f"{name}.caso")

    if "solo_ideal" not in c["flags"]:
        warn("the legacy case uses Γ(ω) images (no solo_ideal); TUPÃ uses ideal images (ADR 0005, ROADMAP P2)")
    if c.get("ar", [0.0, 1.0, 1.0]) != [0.0, 1.0, 1.0]:
        warn(f"legacy air {c['ar']} (sigma epsr mur) replaced by vacuum (ADR 0019)")
    for s in c["skipped"]:
        warn(f"{s} not carried over (no JSON counterpart)")

    src_label, signal = c["signals"][0]
    for label, other in c["signals"][1:]:
        warn(f"additional signal not carried over: node {label} {other}")
    source = st.node_id(src_label)

    observe_nodes = [st.node_id(n) for n in c["nodes"]]
    ids = {e["id"] for e in st.elements}
    observe_electrodes = []
    for k in c["elements"]:
        if f"E{k}" in ids:
            observe_electrodes.append(f"E{k}_e1")
        else:
            warn(f"output element {k} does not exist in the structure — dropped")

    kind, fmax, npts = c["freq"]
    if kind == "freq_log":
        freqs = {"min": float(f"{fmax * 1e-5:.6g}"), "max": fmax, "pointsPerDecade": float(f"{(npts - 1) / 5:.6g}")}
    else:
        freqs = {"min": float(f"{fmax * 1e-4:.6g}"), "max": fmax, "pointsPerDecade": 20}
        warn(f"{kind} {fmax:g} {npts} mapped to a log sweep, 20 points/decade")

    signal.update(sourceNode=source, observeNodes=observe_nodes or [source])
    if observe_electrodes:
        signal["observeElectrodes"] = observe_electrodes
    signal.update(nyquistHz=fmax, fftPoints=c["fft"])
    if c["freq_zero"] is not None:
        signal["freqZeroHz"] = c["freq_zero"]

    outputs = {"nodes": observe_nodes} if observe_nodes else {}
    if observe_electrodes:
        outputs["electrodes"] = observe_electrodes
    outputs["quantities"] = ["voltage", "i1", "i2", "inputImpedance"]

    case = {
        "title": f"Legacy TUPÃ case {name}: {title}",
        "soil": soil_block(c),
        "nodes": [{"id": f"N{k}", "position": list(p)} for k, p in st.nodes.items()],
        "materials": [dict(zip(("id", "epsilonr", "mur", "sigma"), m))
                      for m in MATERIALS.values() if m[0] in st.materials],
        "elements": st.elements,
        "sources": [{"node": source, "current": {"re": 1.0, "im": 0.0}}],
        "frequencies": freqs,
        "outputs": outputs,
        "signal": signal,
    }
    print(f"  {len(st.nodes)} nodes, {len(st.elements)} elements, "
          f"{sum(e['segments'] for e in st.elements)} segments", file=sys.stderr)
    return case


def dumps(case):
    """JSON in the common/ house style: one node/element/material per line."""
    out = ["{"]
    keys = list(case)
    for n, key in enumerate(keys):
        comma = "," if n < len(keys) - 1 else ""
        value = case[key]
        if key in ("nodes", "elements", "materials"):
            items = [json.dumps(v, ensure_ascii=False) for v in value]
            out.append(f'  "{key}": [')
            out += [f"    {s}{',' if k < len(items) - 1 else ''}" for k, s in enumerate(items)]
            out.append(f"  ]{comma}")
        elif key in ("signal", "outputs"):
            fields = [f'    "{k}": {json.dumps(v, ensure_ascii=False)}' for k, v in value.items()]
            out.append(f'  "{key}": {{')
            out.append(",\n".join(fields))
            out.append(f"  }}{comma}")
        else:
            out.append(f'  "{key}": {json.dumps(value, ensure_ascii=False)}{comma}')
    out.append("}")
    return "\n".join(out) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("folder", type=Path, help="folder holding <name>.est and <name>.caso")
    ap.add_argument("names", nargs="+")
    ap.add_argument("--out", type=Path, default=Path("common"))
    ap.add_argument("--title", action="append", default=[],
                    help="English title per case, in the order of the names (the legacy ones are Portuguese); "
                         "default: keep the title of an existing output file")
    args = ap.parse_args()
    for k, name in enumerate(args.names):
        target = args.out / f"{name}.json"
        if k < len(args.title):
            title = args.title[k]
        elif target.exists():
            title = json.loads(target.read_text())["title"].split(": ", 1)[-1]
        else:
            title = "untitled"
        target.write_text(dumps(convert(args.folder, name, title)))


if __name__ == "__main__":
    main()
