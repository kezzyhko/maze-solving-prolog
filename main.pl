:- use_module(library(clpfd)).
:- use_module(library(aggregate)).

:- discontiguous(can_move/5).

:- dynamic(less_moves_yet/2).



% TODO:
%
%	Random search - don't go to walls/orcs - or die?
%	Free move when human
%	Death when orc
%
%	Bitwice operators on MOVE_TYPE to generalize
%
%	Random search - implement many tries - only best solution?
%	Backtracking search - print only best solution?
%	Other algorithms
%
%	Writing output to file
%	Reading input from file
%	Time of execution



% Input

	t(1, 0).

	h(0, 5).
	h(0, 3).

	o(9, 9).



% Constants

	size(3).



% Main

	main :-
		retractall(less_moves_yet(_, _)),
		%% random_search(0, 0, PATH, MOVES), !,
		backtracking_search(0, 0, 0, PATH, MOVES),
		write(PATH),
		write(MOVES).




% Possible moves
	
	on_map(X, Y) :-
		size(SIZE),
		X #>= 0,
		X #=< SIZE - 1,
		Y #>= 0,
		Y #=< SIZE - 1.

	% Step

		can_move(X, Y, NEW_X, NEW_Y, 00) :- can_step(X, Y, NEW_X, NEW_Y,  0,  1). % up
		can_move(X, Y, NEW_X, NEW_Y, 01) :- can_step(X, Y, NEW_X, NEW_Y,  1,  0). % right
		can_move(X, Y, NEW_X, NEW_Y, 02) :- can_step(X, Y, NEW_X, NEW_Y,  0, -1). % down
		can_move(X, Y, NEW_X, NEW_Y, 03) :- can_step(X, Y, NEW_X, NEW_Y, -1,  0). % left

		can_step(X, Y, NEW_X, NEW_Y, DX, DY) :-
			on_map(NEW_X, NEW_Y),
			NEW_X #= X + DX,
			NEW_Y #= Y + DY.


	% Pass

		can_move(X, Y, NEW_X, NEW_Y, 04) :- can_pass(X, Y, NEW_X, NEW_Y,  0,  1). % up
		can_move(X, Y, NEW_X, NEW_Y, 05) :- can_pass(X, Y, NEW_X, NEW_Y,  1,  1). % right-up
		can_move(X, Y, NEW_X, NEW_Y, 06) :- can_pass(X, Y, NEW_X, NEW_Y,  1,  0). % right
		can_move(X, Y, NEW_X, NEW_Y, 07) :- can_pass(X, Y, NEW_X, NEW_Y,  1, -1). % right-down
		can_move(X, Y, NEW_X, NEW_Y, 08) :- can_pass(X, Y, NEW_X, NEW_Y,  0, -1). % down
		can_move(X, Y, NEW_X, NEW_Y, 09) :- can_pass(X, Y, NEW_X, NEW_Y, -1, -1). % left-down
		can_move(X, Y, NEW_X, NEW_Y, 10) :- can_pass(X, Y, NEW_X, NEW_Y, -1,  0). % left
		can_move(X, Y, NEW_X, NEW_Y, 11) :- can_pass(X, Y, NEW_X, NEW_Y, -1,  1). % left-up

		can_pass(X1, Y1, X2, Y2, DX, DY) :-
			can_throw(X1, Y1, X2, Y2, DX, DY),
			h(X2, Y2).

		can_throw(X1, Y1, X2, Y2, DX, DY) :-
			can_step(X1, Y1, X2, Y2, DX, DY).

		can_throw(X1, Y1, X2, Y2, DX, DY) :-
			on_map(X2, Y2),
			K #> 0,
			X2 #= X1 + K*DX,
			Y2 #= Y1 + K*DY,
			PRE_X2 #= X2 - DX,
			PRE_Y2 #= Y2 - DY,
			not(o(PRE_X2, PRE_Y2)),
			not(h(PRE_X2, PRE_Y2)),
			can_throw(X1, Y1, PRE_X2, PRE_Y2, DX, DY).



% Random search

	random_search_many_tries(N, PATH, MOVES) :-
		foreach(between(1, N, _), random_search(0, 0, PATH, MOVES); true).

	random_search(X, Y, [], MOVES) :-
		t(X, Y),
		MOVES #= 0.

	random_search(X, Y, [[NEW_X, NEW_Y] | NEW_PATH], MOVES) :-
		not(t(X, Y)),
		MOVE_TYPE is random(4),
		can_move(X, Y, NEW_X, NEW_Y, MOVE_TYPE),
		random_search(NEW_X, NEW_Y, NEW_PATH, MOVES - 1).



% Backtracking search

	less_moves(POS, MOVES_MADE) :-
		(
			not(less_moves_yet(POS, _));
			less_moves_yet(POS, LEAST_MOVES),
			MOVES_MADE < LEAST_MOVES,
			retractall(less_moves_yet(POS, _))
		),
		assertz(less_moves_yet(POS, MOVES_MADE)).

	backtracking_search(X, Y, MOVES_MADE, [], MOVES_LEFT) :-
		t(X, Y),
		less_moves(touchdown, MOVES_MADE),
		MOVES_LEFT #= 0.

	backtracking_search(X, Y, MOVES_MADE, [[NEW_X, NEW_Y] | NEW_PATH], MOVES_LEFT) :-
		not(t(X, Y)),
		less_moves([X, Y], MOVES_MADE),
		can_move(X, Y, NEW_X, NEW_Y, _),
		backtracking_search(NEW_X, NEW_Y, MOVES_MADE + 1, NEW_PATH, MOVES_LEFT - 1).