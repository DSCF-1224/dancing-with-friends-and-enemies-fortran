load "setting.plt"

set term gif animate delay 2 optimize
set output 'dancers.gif'

set xrange [-2.0:2.0]
set yrange [-2.0:2.0]

set xtics 1
set ytics 1

set size square

unset key

do for [i = 0:num_cycles:save_steps] {

    target_cycle = sprintf("%6.6d", i)
    file_path    = "result/" . target_cycle . ".dat"

    set title target_cycle

    # plot file_path using 1:2 with dots linecolor black
    plot file_path using 1:2 with points linecolor black pointtype 7 pointsize 0.2

}

unset output
