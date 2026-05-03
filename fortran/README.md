# Tupa - Fortran version

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