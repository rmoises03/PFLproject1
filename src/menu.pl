:- consult('io.pl').
:- consult('game.pl').

% menu cycle. Prints title, options and prompts user for input.
% menu/0
menu:-
	print_title,
	repeat,
	print_options,
	read_option(Option),
	start_game(Option),
	!.

% initializes game mode corresponding to option
% start_game(+Option)
start_game('1'):- 
	start_message,
	init_game('h', 'h', GameState),
	game_loop(GameState).
	
start_game('2'):- 
	start_message,
	init_game('cr', 'h', GameState),
	game_loop(GameState).
	
start_game('3'):- 
	start_message,
	init_game('h', 'cr', GameState),
	game_loop(GameState).

start_game('4'):- 
	start_message,
	init_game('cg', 'h', GameState),
	game_loop(GameState).
	
start_game('5'):- 
	start_message,
	init_game('h', 'cg', GameState),
	game_loop(GameState).
	
start_game('6'):- 
	start_message,
	init_game('cr', 'cg', GameState),
	game_loop(GameState).
	
start_game('7'):- 
	start_message,
	init_game('cg', 'cr', GameState),
	game_loop(GameState).

start_game('8'):- 
	start_message,
	init_game('cr', 'cr', GameState),
	game_loop(GameState).
	
start_game('9'):- 
	start_message,
	init_game('cg', 'cg', GameState),
	game_loop(GameState).

start_game('0'):-
    write('Exiting the game. Goodbye!\n'), !.

start_message :-
	write('\nThe game will start\n').
