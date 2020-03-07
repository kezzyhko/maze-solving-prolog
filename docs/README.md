# rugby-prolog
An assignment one on Introduction to AI Spring 2020 (2nd year) course in Innopolis University



## The task

* [Description of the assignment](docs/task.pdf)



## General information

The code is tested only on SWI-Prolog (threaded, 64 bits, version 8.1.21-198-gd129a7435).<br>
The code uses a `clpfd` library for convenient working with integers.
Example of using the code:
* For running random search on `maps/map.pl` map <br>
`swipl -s main.pl -g main -- --map maps/map.pl --alg random_search`
* For help <br>
`swipl -s main.pl -g main -- -h`



## Structure of input files

Each input file should contain:
* Exactly one `size/1` rule
* Exactly one `start/2` rule (usually `start(0, 0)`)
* Any amount of `h/2`, `o/2` and `t/2` rules. However, more than one orc, human and/or touchdown point can’t be at the same coordinates