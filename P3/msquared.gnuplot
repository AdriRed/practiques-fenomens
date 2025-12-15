# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
set terminal png
set output "magne.png"

set ylabel "Magnetization"
set xlabel "Reduced temperature"
set xrange [0:4]
set yrange [0:1.2]
set grid
set key right top
# Dibuja todos los ficheros en una sola línea de comando usando un bucle
plot "files/stats-mc3.dat" using 2:($8/($1*$1)) with points lw 2 title '<|m|>', \
     "files/stats-mc3.dat" using 2:(sqrt($9)/($1*$1)) with points lw 2 title 'sqrt(<m^2>)'