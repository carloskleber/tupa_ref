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


### Linux

