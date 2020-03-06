:- use_module(library(clpfd)).

:- multifile(size/1).
:- multifile(h/2).
:- multifile(o/2).
:- multifile(t/2).

:- dynamic(least_moves_yet/3).

max_path_length(100).



% Command line arguments parsing

main :-

	% parse argv
	AVAILABLE_METHODS = [random_search, backtracking_search, iterative_deepening_search, heuristic_search],
	OPTS_SPEC = [
		[
			opt(help), type(boolean), default(false),
			shortflags([h]), longflags([help]),
			help(['Writes this help text. Other options are ignored when this one is used'])
		],
		[
			opt(input_file), type(atom),
			shortflags([f, i]), longflags([file, input]),
			help(['Path to the file with input. Required'])
		],
		[
			opt(method), type(atom),
			shortflags([a]), longflags([algorithm, alg, method]),
			help(['Method of search to use. Required. Available options:' | AVAILABLE_METHODS])
		]
		% Planning to implement in future:
		%% [
		%% 	opt(max_path_length), type(integer), default(100),
		%% 	shortflags([m]), longflags([max, max_path_length]),
		%% 	help(['Max amount of moves search can make. Works only for random_search and iterative_deepening_search'])
		%% ],
		%% [
		%% 	opt(times), type(integer), default(1),
		%% 	shortflags([n, t]), longflags([amount, number, times]),
		%% 	help(['Amount of times to run the search algorithm. Will output only average time and average path length if not 1'])
		%% ]
	],
	opt_arguments(OPTS_SPEC, OPTS, _),
	member(input_file(INPUT_FILE), OPTS),
	member(method(METHOD), OPTS),

	% check argv
	(
		/* if */ member(help(true), OPTS) ->
		/* then */ (opt_help(OPTS_SPEC, HELP), write(HELP), halt); % help asked
		/* elseif */ var(INPUT_FILE) ->
		/* then */ writeln('Input file is not specified');
		/* elseif */ var(METHOD) ->
		/* then */ writeln('Method of search is not specified');
		/* elseif */ not(member(METHOD, AVAILABLE_METHODS)) ->
		/* then */ writeln('Not valid method name');
		/* else */ (consult(INPUT_FILE), search_and_print(METHOD), halt) % everything is correct
	),

	% if everything is correct, program will halt and the next lines will not be executed
	writeln("Example: 'swipl -s main.pl -g main -- -f input.pl -m random_search'"),
	writeln("Use 'swipl -s main.pl -g main -- -h' for more info"),
	halt.



% Printing results

search_and_print(SEARCH_METHOD) :-
	get_time(START_TIME),
	(( % if path is found
		search(SEARCH_METHOD, PATH)
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
	move_types(TYPE, FUNCTION, _, _, _),
	((FUNCTION = can_pass) -> write('P '); true),
	writef("%w %w\n", [X, Y]),
	print_path(PATH).



% Search methods

% Most of the search methods are looking too similar,
% so there is a general 'search' and particular
% methods are factored out to separate rules.
% itterative deepening is an exception

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
	retractall(visited(_, _)),
	start(X, Y),
	search(SEARCH_METHOD, X, Y, false, 0, RESULT_PATH).

search(_, X, Y, _, _, []) :- % base case for search recursion
	t(X, Y), !.

search(SEARCH_METHOD, X, Y, NO_PASS, MOVES_AMOUNT, PATH) :- % recursive search
	not(o(X, Y)),
	call(SEARCH_METHOD, X, Y, MOVES_AMOUNT, MOVE_TYPE),
	can_move(X, Y, NO_PASS, MOVE_TYPE, NEW_X, NEW_Y),
	move_types(MOVE_TYPE, FUNCTION, _, _, _),
	(( % if we go in cell with human
		h(NEW_X, NEW_Y),
		FUNCTION = can_step
	) -> ( false; % then give free move
		% "false;" is here to fix syntax highlighting bug
		NEW_PATH = PATH,
		NEW_MOVES_AMOUNT is MOVES_AMOUNT
	); ( % else write it in the path
		PATH = [[NEW_X, NEW_Y, MOVE_TYPE] | NEW_PATH],
		NEW_MOVES_AMOUNT is MOVES_AMOUNT + 1
	)),
	search(SEARCH_METHOD, NEW_X, NEW_Y, FUNCTION == can_pass, NEW_MOVES_AMOUNT, NEW_PATH).


random_search(X, Y, MOVES_AMOUNT, MOVE_TYPE) :- % failing too long paths and defining random move
	max_path_length(MAX_MOVES_AMOUNT),
	MOVES_AMOUNT #< MAX_MOVES_AMOUNT,
	bagof(ID, NEW_X^NEW_Y^(can_move(X, Y, false, ID, NEW_X, NEW_Y)), IDS),
	random_member(MOVE_TYPE, IDS).


backtracking_search(X, Y, MOVES_AMOUNT, _) :- % just checking if last move was useful (to optimize)
	(
		not(least_moves_yet(X, Y, _));
		least_moves_yet(X, Y, LEAST_MOVES_AMOUNT),
		MOVES_AMOUNT #< LEAST_MOVES_AMOUNT,
		retractall(least_moves_yet(X, Y, _))
	),
	assertz(least_moves_yet(X, Y, MOVES_AMOUNT)).


heuristic_search(X, Y, MOVES_AMOUNT, MOVE_TYPE) :- % defining order of traverse of moves by heuristic function
	backtracking_search(X, Y, MOVES_AMOUNT, MOVE_TYPE),
	setof([H, MOVE_TYPE], FUNCTION^DX^DY^(move_types(MOVE_TYPE, FUNCTION, DX, DY, H)), SORTED_MOVES), % get all moves and sort them by H
	member([H, MOVE_TYPE], SORTED_MOVES). % split to multiple branches



% Possible moves

%          id  function  dx  dy  H
move_types( 0, can_step,  0,  1, 3). % step up
move_types( 1, can_step,  1,  0, 3). % step right
move_types( 2, can_step,  0, -1, 3). % step down
move_types( 3, can_step, -1,  0, 3). % step left
move_types( 4, can_pass,  0,  1, 2). % pass up
move_types( 5, can_pass,  1,  1, 1). % pass right-up
move_types( 6, can_pass,  1,  0, 2). % pass right
move_types( 7, can_pass,  1, -1, 1). % pass right-down
move_types( 8, can_pass,  0, -1, 2). % pass down
move_types( 9, can_pass, -1, -1, 1). % pass left-down
move_types(10, can_pass, -1,  0, 2). % pass left
move_types(11, can_pass, -1,  1, 1). % pass left-up

can_move(X, Y, NO_PASS, TYPE, NEW_X, NEW_Y) :-
	move_types(TYPE, FUNCTION, DX, DY, _),
	call(FUNCTION, X, Y, NEW_X, NEW_Y, DX, DY),
	(NO_PASS -> FUNCTION \= can_pass; true).

can_step(X, Y, NEW_X, NEW_Y, DX, DY) :-
	move_types(_, can_step, DX, DY, _),
	on_map(NEW_X, NEW_Y),
	NEW_X #= X + DX,
	NEW_Y #= Y + DY.

can_pass(X1, Y1, X2, Y2, DX, DY) :- % base case for check recursion
	move_types(_, can_pass, DX, DY, _),
	X2 #= X1 + DX,
	Y2 #= Y1 + DY,
	h(X2, Y2).

can_pass(X1, Y1, X2, Y2, DX, DY) :- % recursive check
	move_types(_, can_pass, DX, DY, _),
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