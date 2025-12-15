set terminal png
set output "cv.png"

set xlabel "Reduced temperature"
set ylabel "c_v"
set grid
set key left top

# Inicializa variables
prev_x = NaN
prev_y = NaN

# Calcula la derivada numérica
plot "files/stats-mc3.dat" using 2:( \
    diff = (valid($4) && valid(prev_y) && ($2 != prev_x)) ? ($4 - prev_y)/($2 - prev_x) : NaN, \
    prev_x = $2, \
    prev_y = $4, \
    diff/($1*$1) \
) with lines lw 2 title "d<E>/dT", \
    "files/stats-mc3.dat" using 2:(($5-$4*$4)/($2*$2*$1*$1)) with points lw 2 title "c_v"