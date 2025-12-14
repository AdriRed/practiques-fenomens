gfortran -g -O3 -march=native -funroll-loops \
    ./include/mt19937_par.f90 ./include/simulation_stats.f90 ./MC3.f90 \
    -o ./builds/MC3.out