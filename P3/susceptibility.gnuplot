set terminal png
set output "susceptibility.png"

set xlabel "Reduced temperature"
set ylabel "\chi"
set grid
set key left top

Ls = "8 16 24 32 48 64 96 128"
# Calcula la derivada numérica
plot for [i=1:words(Ls)] sprintf("files/stats-%3d-2.dat", word(Ls, i)+0) \
    using 2:(($9-$8*$8)/($2*$1*$1)) with points lw 2 title sprintf("L=%s", word(Ls, i))