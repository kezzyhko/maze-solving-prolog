# rugby-prolog
An assignment one on Introduction to AI Spring 2020 (2nd year) course in Innopolis University



## Attached documents description

* [Description of the assignment](task.pdf)
* [The report](report.pdf)



## General information

The code is tested only on SWI-Prolog (threaded, 64 bits, version `8.1.21-198-gd129a7435`).<br>
The code uses a `clpfd` library for convenient working with integers.<br>
Example of using the code:
* For additional info about available parameters <br>
`swipl -s main.pl -g main -- -h`
* For running random search with defined max path length <br>
`swipl -s main.pl -g main -- --map maps/map.pl --alg random_search --max 500`
* For help running heuristic search with increased vision <br>
`swipl -s main.pl -g main -- --map maps/map.pl --alg heuristic_search -v 2`



## Structure of input files

Each input file should contain:
* Exactly one `size/1` rule
* Exactly one `start/2` rule (usually `start(0, 0)`)
* Any amount of `h/2`, `o/2` and `t/2` rules. However, more than one orc, human and/or touchdown point can’t be at the same coordinates