# How to configure Modern Fortran

1. Install `gfortran` (via MinGW) and `gnuplot` and add them to the PATH

2. Install `fortls`, `findent` and `fprettify` via pip install 

```sh
pip install fortls findent fprettify
```

2. Locate `fortls` install via pip show (idem other packages)

3. Set in configuration the path of the forlts binary (normally under Python3XX/Scripts where XX is your version). If you want to set the fortls path, then put the path of the fortls

4. Create `tasks.json` and `launch.json` files inside `./.vscode/` folder (if not exist in the workspace, then create the `.vscode` folder)

5. Write down the build task and the launch task and adapt-it.

6. Profit

## Compile separated fortran files

For the module:

`gfortran -c {module}.f90`

Then if you want to link the file

`gfortran {code}.f90 {module}.o -o 'outputname'.out`