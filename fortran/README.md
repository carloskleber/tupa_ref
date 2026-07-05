# Tupa - Fortran version

The reference implementation (modern Fortran 2008+, FPM). Project-level
documentation lives in [../docs/](../docs/README.md); conventions in
[../docs/CONVENTIONS.md](../docs/CONVENTIONS.md).

**Compilers**: developed against the latest gfortran; the code is kept
ifx-compatible.

## Recommended setup

### Windows

* Install VS Code;
* Install WSL and a Linux distribution (for the next scripts, I assume you chose Ubuntu);
* Open the Linux distro;
* Install the following packages:

```bash
sudo apt update
sudo apt install gfortran
sudo apt install pipx
pipx install fpm
pipx install fortls
```

* `git clone` from inside your `home/username/` folder;
* `cd tupa_ref/fortran`;
* `code .`;
* Install the recommended "VS Code Server for Linux";
* run `build.sh` (assuming Gfortran) to:
  * get and compile the [SLATEC lib](https://github.com/carloskleber/slatec);
  * Compile the main project with full optimization.
* Run the provided examples with `fpm run --example`.

### Linux

* Go directly to the bash procedure, install fpm and fortls.

#### Arch

Look for `gcc-fortran`

## Building and testing without build.sh

`build.sh` installs SLATEC to `~/.local/lib`, which is not on the default
linker search path. Plain `fpm` invocations therefore need:

```bash
export LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
fpm build
fpm test
```

The SLATEC checkout that `build.sh` clones into `fortran/slatec/` is the
canonical copy (author's fork) and may be fine-tuned in place.

**Test runtimes**: `test_mesh` and `test_assemble` finish in seconds in any
profile. `test_geometry` and `test_impedance` are quadrature-heavy and only
practical under `--profile release` — in the default debug profile they run
for many minutes (see [../docs/ROADMAP.md](../docs/ROADMAP.md) §5). There is
no hosted CI; a local `fpm build && fpm test` is the merge gate.

## Code documentation

Using [FORD](https://forddocs.readthedocs.io/en/stable/). A Python is needed, but only if you need to generated the docs.

Config the enviroment:

### Windows

```powershell
python.exe -m venv .venv
.\.venv\Scripts\Activate.ps1
python.exe -m pip install ford lxml
python.exe -m ford Tupa.md
```

### Linux

```bash
python -m venv .venv
source .venv/bin/activate
pip install ford lxml
python -m ford Tupa.md
```

After that the docs can be accessed in [doc/index.html](doc/index.html).