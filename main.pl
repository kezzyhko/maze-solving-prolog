:- use_module(library(clpfd)).

:- multifile(size/1).
:- multifile(h/2).
:- multifile(o/2).
:- multifile(t/2).
:- include(input).

:- dynamic(least_moves_yet/3).

max_path_length(100).



% Main

main :-
	search_and_print(random),
	search_and_print(dfs),
	search_and_print(iterative_deepening_search).



% Printing results

search_and_print(SEARCH_METHOD) :-
	get_time(START_TIME),
	(( % if path is found
		search_best(SEARCH_METHOD, PATH)
	) -> ( false; % then output it
		% "false;" is here to fix syntax highlighting bug
		get_time(END_TIME),
		length(PATH, MOVES_AMOUNT),
		writeln(MOVES_AMOUNT),
		print_path(PATH)
	); ( % else output error message
		get_time(END_TIME),
		writeln("No path has been found")
	)),
	TIME is (END_TIME - START_TIME) * 1000,
	format("~3f msec~n", [TIME]).

print_path([]).
print_path([[X, Y, TYPE] | PATH]) :-
	move_types(TYPE, FUNCTION, _, _),
	((FUNCTION = can_pass) -> write("P "); true),
	writef("%w %w\n", [X, Y]),
	print_path(PATH).



% Selecting best path from search

search_best(dfs, PATH) :- % aggregating all solutions to find the best one
	aggregate(min(MOVES_AMOUNT, P), (search(backtracking_search, P), length(P, MOVES_AMOUNT)), min(MOVES_AMOUNT, PATH)).

search_best(METHOD, PATH) :- % returning just first solution for methods defined in the list
	member(METHOD, [random_search, iterative_deepening_search]),
	search(METHOD, PATH),
	!.



% Search methods

% Most of the search methods are looking too similar,
% so there is a general 'search' and particular
% methods are factored out to separate rules.
% iterative_deepening_search is an exception

search(iterative_deepening_search, RESULT_PATH) :-
	max_path_length(MAX_MOVES_AMOUNT),
	length(RESULT_PATH, MOVES_AMOUNT),
	(
		MOVES_AMOUNT =< MAX_MOVES_AMOUNT;
		MOVES_AMOUNT > MAX_MOVES_AMOUNT, !, fail
	),
	retractall(least_moves_yet(_, _, _)),
	search(backtracking_search, RESULT_PATH).

search(SEARCH_METHOD, RESULT_PATH) :- % shortcut
	retractall(least_moves_yet(_, _, _)),
	start(X, Y),
	search(SEARCH_METHOD, X, Y, 0, false, RESULT_PATH).

search(_, X, Y, _, _, []) :- % base case for search recursion
	t(X, Y).

search(SEARCH_METHOD, X, Y, MOVES_AMOUNT, NO_PASS, [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH]) :- % recursive search
	not(t(X, Y)),
	not(o(X, Y)),
	call(SEARCH_METHOD, X, Y, MOVES_AMOUNT, MOVE_TYPE),
	can_move(X, Y, NO_PASS, MOVE_TYPE, NEW_X, NEW_Y),
	move_types(MOVE_TYPE, FUNCTION, _, _),
	search(SEARCH_METHOD, NEW_X, NEW_Y, MOVES_AMOUNT + 1, FUNCTION == can_pass, NEW_PATH).

random_search(_, _, MOVES_AMOUNT, MOVE_TYPE) :- % failing too long paths and defining random move
	max_path_length(MAX_MOVES_AMOUNT),
	MOVES_AMOUNT #< MAX_MOVES_AMOUNT,
	aggregate_all(max(ID), move_types(ID, _, _, _), MAX_TYPE),
	MOVE_TYPE is random(MAX_TYPE+1).

backtracking_search(X, Y, MOVES_AMOUNT, _) :- % just checking if move is useful to optimize
	(
		not(least_moves_yet(X, Y, _));
		least_moves_yet(X, Y, LEAST_MOVES_AMOUNT),
		MOVES_AMOUNT #< LEAST_MOVES_AMOUNT,
		retractall(least_moves_yet(X, Y, _))
	),
	assertz(least_moves_yet(X, Y, MOVES_AMOUNT)).



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

can_move(X, Y, NO_PASS, TYPE, NEW_X, NEW_Y) :-
	move_types(TYPE, FUNCTION, DX, DY),
	call(FUNCTION, X, Y, NEW_X, NEW_Y, DX, DY),
	(NO_PASS -> FUNCTION \= can_pass; true).

can_move(X, Y, NO_PASS, TYPE, NEW_X, NEW_Y) :- % Free move when we go in cell with human
	can_step(X, Y, MID_X, MID_Y, _, _),
	h(MID_X, MID_Y),
	can_move(MID_X, MID_Y, NO_PASS, TYPE, NEW_X, NEW_Y).

can_step(X, Y, NEW_X, NEW_Y, DX, DY) :-
	move_types(_, can_step, DX, DY),
	on_map(NEW_X, NEW_Y),
	NEW_X #= X + DX,
	NEW_Y #= Y + DY.

can_pass(X1, Y1, X2, Y2, DX, DY) :- % base case for check recursion
	move_types(_, can_pass, DX, DY),
	X2 #= X1 + DX,
	Y2 #= Y1 + DY,
	h(X2, Y2).

can_pass(X1, Y1, X2, Y2, DX, DY) :- % recursive check
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