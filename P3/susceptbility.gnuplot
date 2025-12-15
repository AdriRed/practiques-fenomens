set terminal png
set output "susceptibility.png"

set xlabel "Reduced temperature"
set ylabel "\chi"
set grid
set key left top

# Calcula la derivada numérica
plot "files/stats-mc3-  8.dat" using 2:(($9-$8*$8)/($2*$1*$1)) with points lw 2 title "L=8", \
"files/stats-mc3- 16.dat" using 2:(($9-$8*$8)/($2*$1*$1)) with points lw 2 title "L=16", \
"files/stats-mc3- 32.dat" using 2:(($9-$8*$8)/($2*$1*$1)) with points lw 2 title "L=32", \
"files/stats-mc3- 64.dat" using 2:(($9-$8*$8)/($2*$1*$1)) with points lw 2 title "L=64", \