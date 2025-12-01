# gnuplot script para pintar varios ficheros de energía
# Guardar como plot_energy.gnu y ejecutar: gnuplot plot_energy.gnu
# Si quieres ver la ventana en vez de solo guardar PNG, usa `set term qt` y quita/setea output.

# Parámetros de los ficheros (fáciles de editar)
temps = "1.500 1.800 2.500 3.500 4.500"
prefix = "files/SIM-L48-TEMP"
suffix = "-MCTOT10000.dat"

set terminal pngcairo size 1200,600 enhanced font "Arial,10"
set output "magne.png"

set title "Magnetización vs paso de Monte Carlo"
set xlabel "Paso"
set ylabel "Magnetización"
set xrange [0:10000]
set grid

# Dibuja todos los ficheros en una sola línea de comando usando un bucle
plot for [i=1:words(temps)] \
    sprintf("%s%s%s", prefix, word(temps,i), suffix) using 1:4 with lines lw 2 title sprintf("T=%s", word(temps,i))

set key left top
