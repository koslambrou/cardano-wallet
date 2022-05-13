#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gawk time haskellPackages.hp2pretty buildkite-agent gnuplot

set -euo pipefail

network=$1

# relativize timelogs into dats


# draw the block ingestion vs time graph 

GNUPLOT_PROGRAM=$(cat <<EOP
set timefmt "%s";
set format x "%Hh%Mm%Ss";
set xdata time;

set xlabel "time";
set ylabel "block height";
show xlabel;
show ylabel;

set terminal svg dynamic size 1200,700 background rgb 'white';
set output "block ingestion.svg";
set title "Restoring wallets on $network, block ingestion";

set key left top;
set grid y my x mx;
set grid;


FILES = system("ls -1 *.timelog");
LABEL = system("ls -1 *.timelog");
rel(x) = (\$0 == 0) ? (x0 = x,0) : (x - x0);
plot for [i=1:words(FILES)] word(FILES,i) u (rel(\$1)):(\$2) title word(LABEL,i) noenhanced with lines

EOP
);

# Plots all .log files in a single plot;
echo $GNUPLOT_PROGRAM | gnuplot


# draw throughput graph

GNUPLOT_PROGRAM=$(cat <<EOP

set xlabel "block height";
set ylabel "block throughput, blocks/s";
show xlabel;
show ylabel;


set terminal svg dynamic size 1200,700 background rgb 'white';
set output "blocks-throughput.svg";
set title "Restoring wallets on $network, block throughputs";


set logscale y;
set grid y my x mx;
set key left top;


derivative(x,y) = (\$0 == 0) ? (x1 = x, y1 = y, 1/0) : (x2 = x1, x1 = x, y2 = y1, y1 = y, (y1-y2)/(x1-x2));



FILES = system("ls -1 *.timelog");
LABEL = system("ls -1 *.timelog");

plot for [i=1:words(FILES)] word(FILES,i) u (\$2):(derivative(\$1,\$2)) smooth sbezier title word(LABEL,i) noenhanced with lines
EOP
);

# Plots all .log files in a single plot;
echo $GNUPLOT_PROGRAM | gnuplot