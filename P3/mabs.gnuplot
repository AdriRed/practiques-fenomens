# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
set terminal png
set output "magneabs.png"

set ylabel "<|M|>"
set xlabel "T"
set xrange [1:5]
set yrange [0:1.2]
set grid
set key right top

Ls = "8 16 24 32 48 64 96 128"

plot for [i=1:words(Ls)] sprintf("files/stats-mc3-%3d.dat", word(Ls, i)+0) using 2:($8/($1*$1)) with points lw 2 title sprintf('L = %d', word(Ls, i)+0)