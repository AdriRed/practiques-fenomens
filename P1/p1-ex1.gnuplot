set term png
set out "ex1.png"
set boxwidth 1
set xrange [-1:10]
plot "builds/p1-ex1.dat" u 3:(1) smooth freq w boxes