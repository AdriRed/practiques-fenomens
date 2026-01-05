# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
set terminal png
set output "magne.png"

set ylabel "Magnetization"
set xlabel "Reduced temperature"
set xrange [1:5]
set yrange [0:1.2]
set grid
set key right top

Ls = "8 16 24 32 48 64 96 128"

plot for [i=1:words(Ls)] sprintf("files/stats-%3d-2.dat", word(Ls, i)+0) using 2:($8/($1*$1)) with points lw 2 title sprintf('<|m|> L = %d', word(Ls, i)+0), \
     for [i=1:words(Ls)] sprintf("files/stats-%3d-2.dat", word(Ls, i)+0) using 2:(sqrt($9)/($1*$1)) with points lw 2 title sprintf('sqrt(<m^2>) L = %d',word(Ls, i)+0)
