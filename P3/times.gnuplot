set terminal png size 1200,800 enhanced font "Arial,14"
set output "times.png"

set xlabel "L" offset 1,-0.8
set ylabel "MCTOT" offset 8,0
set zlabel "Time (s)"

set xtics 8 offset 0, -0.5
set xrange [97:7]
set grid
set ytics 2000
set yrange [1000:10000]
set ticslevel 0 
set zrange [0.01:*] 
set autoscale zmax  # Ajusta automáticamente el máximo de Z
# set wall z0
set logscale z
set hidden3d
set grid xtics ytics ztics layerdefault
set xyplane at -1  # Posiciona el plano xy en z=-1
splot for [i=1:12] \
    sprintf("< sort 'files/times-%2d .dat' | awk '{print $1,$2,0.01; print $1,$2,$4; print \"\"}'", i*8) \
    using 1:2:3 with lines notitle