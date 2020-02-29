:- use_module(library(clpfd)).



% TODO:
%
%	Random search - many tries
%	Random search - don't go to walls/orcs - or die?
%	Other algorithms
%
%	Passing the ball
%	Free move when human
%	Death when orc
%
%	Writing output to file
%	Reading input from file



% Input

t(1, 0).



% Constants

size(3).



% Main

main :-
	random_search(0, 0, PATH, MOVES), !,
	write(PATH),
	write(MOVES).




% Useful rules

on_map(X, Y) :-
	size(MAX),
	X #>= 0,
	X #< MAX,
	Y #>= 0,
	Y #< MAX.

neighbours(X1, Y1, X2, Y2) :-
	X1 = X2, (Y1 #= Y2 + 1; Y1 #= Y2 - 1);
	Y1 = Y2, (X1 #= X2 + 1; X1 #= X2 - 1).



% Random search

random_search(X, Y, [[X, Y]], MOVES) :-
	t(X, Y),
	MOVES #= 0.

random_search(X, Y, [[X, Y] | NEW_PATH], MOVES) :-
	not(t(X, Y)),
	random(1, 4, MOVE),
	(
		(MOVE = 1), (NEW_X = X, NEW_Y is Y + 1);
		(MOVE = 2), (NEW_X = X, NEW_Y is Y - 1);
		(MOVE = 3), (NEW_Y = Y, NEW_X is X + 1);
		(MOVE = 4), (NEW_Y = Y, NEW_X is X - 1)
	),
	on_map(NEW_X, NEW_Y),
	random_search(NEW_X, NEW_Y, NEW_PATH, MOVES - 1).