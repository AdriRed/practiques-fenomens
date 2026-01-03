set terminal png
set output "susceptibility-rescaling.png"

set xlabel "Reduced temperature"
set ylabel "\chi"
set grid
set key left top
set yrange [0:100]
Ls = "8 16 24 32 48 64 96 128"
plot for [i=1:words(Ls)] sprintf("files/stats-mc3-%3d.dat", word(Ls, i)+0) \
    using (($2-2.3)/2.3*$1**0.25):(($9-$8*$8)/($2*$1**1.75)) with points lw 1 title sprintf('L=%s', word(Ls, i))