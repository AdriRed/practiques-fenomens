gfortran -O3 -march=native -funroll-loops \
    ./include/mt19937.f90 ./include/simulation_stats.f90 ./MC2.f90 \
    -o ./builds/MC2.out