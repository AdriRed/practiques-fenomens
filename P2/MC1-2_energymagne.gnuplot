# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
temps = "1.7 2.1 2.4 2.5 2.8 3.2"
prefix = "files/SIM-L92-TEMP"
suffix = "-MCTOT8000.dat"

set terminal pngcairo size 900,450 enhanced font "Arial,10"
set output "energymagne.png"

set multiplot layout 2, 1

set ylabel "Magnetización"
set xrange [0:8000]
set key horiz
set key center top
set key outside 
# set key left bottom
set yrange [-10000: 10000]
# Dibuja todos los ficheros en una sola línea de comando usando un bucle
plot for [i=1:words(temps)] \
    sprintf("%s%.3f%s", prefix, word(temps,i)+0, suffix) using 1:4 with lines lw 2 title sprintf("T=%s", word(temps,i))


set yrange [-20000:0 ]

# set key left top
unset key
set xlabel "Paso"
set ylabel "Energía"
set xrange [0:8000]

plot for [i=1:words(temps)] \
    sprintf("%s%.3f%s", prefix, word(temps,i)+0, suffix) using 1:2 with lines lw 2 title sprintf("T=%s", word(temps,i))

unset multiplot
