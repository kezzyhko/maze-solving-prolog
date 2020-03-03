:- use_module(library(clpfd)).
:- multifile(size/1).
:- multifile(h/2).
:- multifile(o/2).
:- multifile(t/2).
:- include(input).
:- dynamic(least_moves_yet/2).



% Main

	max_path_length(random_search, 100).
	max_path_length(itterative_deepening, 15).

	main :-
		retractall(least_moves_yet(_, _)),
		get_time(START_TIME),
		(( % if path is found
			%% random_search(0, 0, 0, false, PATH)	% random search
			%% aggregate(min(MOVES, P), (backtracking_search(0, 0, 0, false, P), length(P, MOVES)), min(MOVES, PATH)) % dfs
			max_path_length(itterative_deepening, MAX_MOVES), MOVES #=< MAX_MOVES, length(PATH, MOVES), retractall(least_moves_yet(_, _)), backtracking_search(0, 0, 0, false, PATH) % itterative deepening dfs
		) -> ( % then output it
			length(PATH, MOVES),
			writeln(MOVES),
			print_path(PATH)
		); ( % else output error message
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

	can_move(X, Y, NEW_X, NEW_Y, NO_PASS, MOVE_TYPE) :-
		move_types(MOVE_TYPE, FUNCTION, DX, DY),
		call(FUNCTION, X, Y, NEW_X, NEW_Y, DX, DY),
		(NO_PASS -> FUNCTION \= can_pass; true).

	can_move(X, Y, NEW_X, NEW_Y, NO_PASS, TYPE) :- % Free move when go in cell with human
		h(MID_X, MID_Y),
		can_step(X, Y, MID_X, MID_Y, _, _),
		can_move(MID_X, MID_Y, NEW_X, NEW_Y, NO_PASS, TYPE).

	can_step(X, Y, NEW_X, NEW_Y, DX, DY) :-
		move_types(_, can_step, DX, DY),
		on_map(NEW_X, NEW_Y),
		NEW_X #= X + DX,
		NEW_Y #= Y + DY.

	can_pass(X1, Y1, X2, Y2, DX, DY) :-
		move_types(_, can_pass, DX, DY),
		X2 #= X1 + DX,
		Y2 #= Y1 + DY,
		h(X2, Y2).

	can_pass(X1, Y1, X2, Y2, DX, DY) :-
		move_types(_, can_pass, DX, DY),
		on_map(X1, Y1),
		NEXT_X #= X1 + DX,
		NEXT_Y #= Y1 + DY,
		can_pass(NEXT_X, NEXT_Y, X2, Y2, DX, DY),
		not(h(NEXT_X, NEXT_Y)),
		not(o(NEXT_X, NEXT_Y)).
	
	on_map(X, Y) :-
		size(SIZE),
		X #>= 0,
		X #=< SIZE - 1,
		Y #>= 0,
		Y #=< SIZE - 1.



% Random search

	random_search(X, Y, MOVES, _, []) :-
		t(X, Y),
		max_path_length(random_search, MAX_MOVES),
		MOVES =< MAX_MOVES.

	random_search(X, Y, MOVES, NO_PASS, [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH]) :-
		max_path_length(random_search, MAX_MOVES),
		MOVES =< MAX_MOVES,
		not(t(X, Y)),
		not(o(X, Y)),
		aggregate_all(max(ID), move_types(ID, _, _, _), MAX_TYPE),
		MOVE_TYPE is random(MAX_TYPE+1),
		can_move(X, Y, NEW_X, NEW_Y, NO_PASS, MOVE_TYPE),
		move_types(MOVE_TYPE, FUNCTION, _, _),
		NEW_NO_PASS = (FUNCTION == can_pass),
		random_search(NEW_X, NEW_Y, MOVES + 1, NEW_NO_PASS, NEW_PATH).



% Backtracking search

	less_moves(POS, MOVES) :-
		(
			not(least_moves_yet(POS, _));
			least_moves_yet(POS, LEAST_MOVES),
			MOVES #< LEAST_MOVES,
			retractall(least_moves_yet(POS, _))
		),
		assertz(least_moves_yet(POS, MOVES)).

	backtracking_search(X, Y, MOVES, _, []) :-
		t(X, Y),
		less_moves(touchdown, MOVES).

	backtracking_search(X, Y, MOVES, NO_PASS, [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH]) :-
		not(t(X, Y)),
		not(o(X, Y)),
		less_moves([X, Y], MOVES),
		can_move(X, Y, NEW_X, NEW_Y, NO_PASS, MOVE_TYPE),
		move_types(MOVE_TYPE, FUNCTION, _, _),
		NEW_NO_PASS = (FUNCTION == can_pass),
		backtracking_search(NEW_X, NEW_Y, MOVES + 1, NEW_NO_PASS, NEW_PATH).