# rugby-prolog
Labirinth solving implemented in prolog. 

Originally, it was an assignment one on Introduction to AI Spring course in Innopolis University (2020, 2nd year).<br>
The original report with more information can be found [here](https://docs.google.com/document/d/1xCakI2pGhRruwe632Ot1qa0QZOMnMIx42O8RP8UW83s).



## Table of contents

* [Example of usage](#example-of-usage)
* [Structure of input files](#structure-of-input-files)
* [Other information](#other-information)



## Example of usage
* For additional info about available parameters <br>
`swipl -s main.pl -g main -- -h`
* For running random search with defined max path length <br>
`swipl -s main.pl -g main -- --map maps/map.pl --alg random_search --max 500`
* For help running heuristic search with increased vision <br>
`swipl -s main.pl -g main -- --map maps/map.pl --alg heuristic_search -v 2`



## Structure of input files

Each input file should contain:
* Exactly one `size/1` rule, which defines size of the map. For example, `size(5)` would mean that map is 5 by 5.
* Exactly one `start/2` rule, which defines on which cell maze starts. Usually it is `start(0, 0)`
* Any amount of `o/2` rules. Defines the unpassable wall.
* Any amount of `t/2` rules. Defined the end points of the maze.
* Any amount of `h/2` rules. Defines the "teleport" field. Player can teleport to them from any distance, if such a field is on the same line with player. Also works diagonally.
However, more than one `o/2`, `h/2` and/or `t/2` can’t be at the same coordinates



## Other information

The code is tested only on SWI-Prolog (threaded, 64 bits, version `8.1.21-198-gd129a7435`).<br>
The code uses a `clpfd` library for convenient working with integers.<br>