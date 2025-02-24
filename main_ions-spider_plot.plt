set encoding utf8

$DATA << EOD
"Jony główne" "Ca^2^+"   "Mg^2^+"   "HCO_3^-"   "SO_4^2^-"  "Cl^-"  "Na^+ + K^+ "
"Próbka LP=40" 83.894     6.032      73.763       19.309   6.926         7.685
"Próbka LP=41" 83.617     8.372      75.633       19.063   5.116         7.655
"Próbka LP=42" 83.105     8.080      92.366       4.909    2.661         7.593
EOD

set spiderplot
set style spiderplot fs transparent solid 0.2 border
set for [p=1:6] paxis p range [0:100]
set for [p=1:6] paxis p tics font ",9"
set grid spiderplot
plot for [i=2:7] $DATA using i:key(1) title columnhead
