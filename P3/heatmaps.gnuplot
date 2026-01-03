
set terminal pngcairo
set output "spins.png"

set tmargin 0
set bmargin 0

set multiplot layout 2, 5 \
    scale 1.5, 1.5 \
    # margins 0.01, 10, 10, 0.01 \
    spacing 0.01, 0.01
# set margin 0

unset key
unset colorbox
set tic scale 0

# Color runs from white to green
set palette rgbformula 21,22,2
set cbrange [-1:1]
# set cblabel "Score"
# unset cbtics

set xrange [-0.5:31.5]
set yrange [-0.5:31.5]

unset xtics
unset ytics


set size ratio -1
set view map
splot './files/spins-start-1000.dat' matrix with image
splot './files/spins-start-1001.dat' matrix with image
splot './files/spins-start-1002.dat' matrix with image
splot './files/spins-start-1003.dat' matrix with image
splot './files/spins-start-1004.dat' matrix with image

splot './files/spins-end-1000.dat' matrix with image
splot './files/spins-end-1001.dat' matrix with image
splot './files/spins-end-1002.dat' matrix with image
splot './files/spins-end-1003.dat' matrix with image
splot './files/spins-end-1004.dat' matrix with image