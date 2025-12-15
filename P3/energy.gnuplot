# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
set terminal png
set output "energy.png"

set ylabel "Energy"
set xlabel "Reduced temperature"
set xrange [0:4]
set yrange [-2:0]
set grid

# Dibuja todos los ficheros en una sola línea de comando usando un bucle
plot "files/stats-mc3-  8.dat" using 2:($4/($1*$1)) with points lw 1 title 'L=8',\
 "files/stats-mc3- 16.dat" using 2:($4/($1*$1)) with points lw 1 title 'L=16',\
  "files/stats-mc3- 32.dat" using 2:($4/($1*$1)) with points lw 1 title 'L=32',\
   "files/stats-mc3- 64.dat" using 2:($4/($1*$1)) with points lw 1 title 'L=64'
set key left top
