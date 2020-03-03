:- use_module(library(clpfd)).
:- include(input).
:- discontiguous(can_move/5).
:- dynamic(less_moves_yet/2).



% Main

	main :-
		retractall(less_moves_yet(_, _)),
		get_time(START_TIME),
		(( % if
			%% random_search(0, 0, 0, PATH), !
			backtracking_search(0, 0, 0, PATH)
		) -> ( % then
			length(PATH, MOVES),
			writeln(MOVES),
			print_path(PATH)
		); (
			writeln("No path has been found")
		)),
		get_time(END_TIME),
		TIME is (END_TIME - START_TIME) * 1000,
		format("~3f msec~n", [TIME]).

	print_path([]).
	print_path([[X, Y, TYPE] | PATH]) :-
		move_types(TYPE, FUNCTION, _, _),
		((FUNCTION = can_pass) -> write("P "); true),
		writef("%w %w\n", [X, Y]),
		print_path(PATH).




% Possible moves

	%          id  function  dx  dy
	move_types( 0, can_step,  0,  1). % step up
	move_types( 1, can_step,  1,  0). % step right
	move_types( 2, can_step,  0, -1). % step down
	move_types( 3, can_step, -1,  0). % step left
	move_types( 4, can_pass,  0,  1). % pass up
	move_types( 5, can_pass,  1,  1). % pass right-up
	move_types( 6, can_pass,  1,  0). % pass right
	move_types( 7, can_pass,  1, -1). % pass right-down
	move_types( 8, can_pass,  0, -1). % pass down
	move_types( 9, can_pass, -1, -1). % pass left-down
	move_types(10, can_pass, -1,  0). % pass left
	move_types(11, can_pass, -1,  1). % pass left-up

	can_move(X, Y, NEW_X, NEW_Y, MOVE_TYPE) :-
		move_types(MOVE_TYPE, FUNCTION, DX, DY),
		call(FUNCTION, X, Y, NEW_X, NEW_Y, DX, DY).

	can_move(X, Y, NEW_X, NEW_Y, TYPE) :- % Free move when go in cell with human
		h(MID_X, MID_Y),
		can_step(X, Y, MID_X, MID_Y, _, _),
		can_move(MID_X, MID_Y, NEW_X, NEW_Y, TYPE).

	can_step(X, Y, NEW_X, NEW_Y, DX, DY) :-
		abs(DX) + abs(DY) #= 1,
		on_map(NEW_X, NEW_Y),
		NEW_X #= X + DX,
		NEW_Y #= Y + DY.

	can_pass(X1, Y1, X2, Y2, DX, DY) :-
		can_throw(X1, Y1, X2, Y2, DX, DY),
		h(X2, Y2).

	can_throw(X1, Y1, X2, Y2, DX, DY) :-
		abs(DX) #= 1,
		abs(DY) #= 1,
		on_map(X2, Y2),
		X2 #= X1 + DX,
		Y2 #= Y1 + DY.

	can_throw(X1, Y1, X2, Y2, DX, DY) :-
		abs(DX) #= 1,
		abs(DY) #= 1,
		on_map(X2, Y2),
		K #> 0,
		X2 #= X1 + K*DX,
		Y2 #= Y1 + K*DY,
		PRE_X2 #= X2 - DX,
		PRE_Y2 #= Y2 - DY,
		can_throw(X1, Y1, PRE_X2, PRE_Y2, DX, DY),
		not(h(PRE_X2, PRE_Y2)),
		not(o(PRE_X2, PRE_Y2)).
	
	on_map(X, Y) :-
		size(SIZE),
		X #>= 0,
		X #=< SIZE - 1,
		Y #>= 0,
		Y #=< SIZE - 1.



% Random search

	random_search(X, Y, MOVES, []) :-
		t(X, Y),
		MOVES =< 100.

	random_search(X, Y, MOVES, [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH]) :-
		MOVES < 100,
		not(t(X, Y)),
		not(o(X, Y)),
		MOVE_TYPE is random(4),
		can_move(X, Y, NEW_X, NEW_Y, MOVE_TYPE),
		random_search(NEW_X, NEW_Y, MOVES + 1, NEW_PATH).



% Backtracking search

	less_moves(POS, MOVES) :-
		(
			not(less_moves_yet(POS, _));
			less_moves_yet(POS, LEAST_MOVES),
			MOVES #< LEAST_MOVES,
			retractall(less_moves_yet(POS, _))
		),
		assertz(less_moves_yet(POS, MOVES)).

	backtracking_search(X, Y, MOVES, []) :-
		t(X, Y),
		less_moves(touchdown, MOVES).

	backtracking_search(X, Y, MOVES, [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH]) :-
		not(t(X, Y)),
		not(o(X, Y)),
		less_moves([X, Y], MOVES),
		can_move(X, Y, NEW_X, NEW_Y, MOVE_TYPE),
		backtracking_search(NEW_X, NEW_Y, MOVES + 1, NEW_PATH).