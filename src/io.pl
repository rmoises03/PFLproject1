:-use_module(library(lists)).
:-consult('tools.pl').

% print board
% display_board(+Type, +Board)
display_board('h', Board):-
	display_board_aux(Board),
	nl.

display_board('cr', Board):-
	display_board_aux(Board),
	nl,
	write('Press any key to continue\n'),
	get_char(_).
	
display_board('cg', Board):-
	display_board_aux(Board),
	nl,
	write('Press any key to continue\n'),
	get_char(_).
	
% display_board_aux(+Board)
display_board_aux([]).
display_board_aux([H|T]):-
	print_row(H),
	display_board_aux(T).

% print row
% print_row(+Row)
print_row([]) :- nl. 
print_row([H|T]):-
    print(H),
	print_row(T).

% displays gamestate
% display_game(+GameState)
display_game(gameState(Board, player(Turn, Type , Piece), _, Turn)):-
	format('~nTurn of ~w~n', [Piece]),
	nl,
	length(Board, Size),
	display_move_direction(Size),
	display_board(Type, Board).
	
display_game(gameState(Board, _, player(Turn, Type, Piece), Turn)):-
	format('~nTurn of ~w~n', [Piece]),
	nl,
	length(Board, Size),
	display_move_direction(Size),
	display_board(Type, Board).

% displays information related to the Board
% display_move_direction(+Size)
display_move_direction(Size):-
	MaxIndex is Size-1,
	format('X: left->right (0->~w) | ', [MaxIndex]),
	format('Y: up->down (0->~w)~n', [MaxIndex]),
	nl.


% Prints the menu title
% print_title/0
print_title :-
    nl,
  	write('\t   _____ _              _    _  _____ _______ _____   ____  \n'),
    write('\t  / ____| |        /\\  | |  | |/ ____|__   __|  __ \\ / __ \\ \n'),
	write('\t | |    | |       /  \\ | |  | | (___    | |  | |__) | |  | |\n'),
	write('\t | |    | |      / /\\ \\| |  | |\\___ \\   | |  |  _  /| |  | |\n'),
	write('\t | |____| |____ / ____ \\ |__| |____) |  | |  | | \\ \\| |__| |\n'),
	write('\t  \\_____|______/_/    \\_\\____/|_____/   |_|  |_|  \\_\\\\____/ \n'),
    nl.


option('1', 'Human vs Human').
option('2', 'Computer Random vs Human').
option('3', 'Human vs Computer Random').
option('4', 'Computer Greedy vs Human').
option('5', 'Human vs Computer Greedy').
option('6', 'Computer Random vs Computer Greedy').
option('7', 'Computer Greedy vs Computer Random').
option('8', 'Computer Random vs Computer Random').
option('9', 'Computer Greedy vs Computer Greedy').
option('0', 'Exit').

% Prints the options
print_options :-
	findall([A]-B, option(A, B), Options),
	print_options_aux(Options).

% Receives and prints a list of the menu options
% print_options(+Options)
print_options_aux([]).
print_options_aux([X|XS]) :-
	write(X),
	nl,
	print_options_aux(XS).

% read user option from the menu
% read_option(-Option)
read_option(H) :-
	write('Please write a valid option\n'),
	read_keyboard([H|_]),
	findall(A, option(A, _), Options),
	member(H, Options),
	!.

% read and validate user move	
% read_move(-MoveOption)
read_move(MoveOption):-
	repeat,
	read_keyboard(Input),
	validate_input(Input, MoveOption),
	!.

% read from keyboard
% read_keyboard(-Input)
read_keyboard(Input):-
	nl,
	get_code(Code),
	read_keyboard_aux(Code, Input),
	!.

% read_keyboard_aux(+Ch, -Input)
read_keyboard_aux(13, []).
read_keyboard_aux(10, []).
read_keyboard_aux(Code, [Ch|Rest]):-
	get_code(Code1),
	char_code(Ch, Code),
	read_keyboard_aux(Code1, Rest).

% validate user input
% validate_input(+Input, -Moves)
validate_input(Input, Moves):-
	delete(Input, ' ', Input1),
	length(Input1, Len),
	!,
	Len > 0,
	validate_input_aux(Input1, Moves).

% validate_input_aux(+Input, -Moves)
validate_input_aux([], []).
validate_input_aux([X|T], Moves) :-
    verify_number(X, NX), % Verify the first digit
    validate_input_aux(T, Rest), % Recursive call for the rest of the input
    % Concatenate the verified number with the rest of the input
    append([NX], Rest, Moves).


% print_possible_moves_piece(+Position, +Moves, +OptionNumber, -NextOptionNumber)
print_possible_moves_piece(_, [], OptionNumber, OptionNumber).
print_possible_moves_piece(X-Y, [NX-NY|T], OptionNumber, NextOptionNumber) :-
	format('~d: Move from (~d,~d) to (~d,~d)', [OptionNumber, X, Y, NX, NY]), nl,
    NextOption is OptionNumber + 1,
    print_possible_moves_piece(X-Y, T, NextOption, NextOptionNumber).

% print_possible_moves(+Moves, +OptionNumber, -NextOptionNumber)
print_possible_moves([], OptionNumber, OptionNumber).
print_possible_moves([[X-Y|ValidMoves]|Rest], OptionNumber, NextOptionNumber) :-
    print_possible_moves_piece(X-Y, ValidMoves, OptionNumber, NextOption),
    print_possible_moves(Rest, NextOption, NextOptionNumber).

% print_possible_moves(+Moves, +Counter)
print_reput_options([], _).
print_reput_options([H|T], Counter) :-
	format('~d: Reput enemy in ~w~n', [Counter, H]),
	Counter1 is Counter + 1,
	print_reput_options(T, Counter1).