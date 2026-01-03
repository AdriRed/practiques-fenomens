set terminal png
set output "cv.png"

set xlabel "Reduced temperature"
set ylabel "c_v"
set grid
set key right top
Ls = "8 16 24 32 48 64 96 128"

plot for [i=1:words(Ls)] sprintf("files/stats-mc3-%3d.dat", word(Ls, i)+0) using 2:(($5-$4*$4)/($2*$2*$1*$1)) with points lw 1 title sprintf('L=%s', word(Ls, i))
