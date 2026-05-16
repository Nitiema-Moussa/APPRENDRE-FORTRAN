 set title 'Evolution de T_A1 et T_A2'
 set xlabel 'Temps (minutes)'
 set ylabel 'Température (°C)'
 plot 'data.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', 'data.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'
