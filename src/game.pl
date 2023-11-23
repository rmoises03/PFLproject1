:-use_module(library(lists)).
:-use_module(library(random)).

:-consult('board.pl').
:-consult('tools.pl').

% initialize game
% init_game(+Type1, +Type2, -GameState)
init_game(Type1, Type2, gameState(NewBoard, player(1, Type1, 'P1'), player(2, Type2, 'P2'), 1)) :-
	init_board(NewBoard).

% next_turn(+Last, -Next)
next_turn(1, 2).
next_turn(2, 1).

% get_pieces(+Board, +Piece, -Pieces)
get_pieces(Board, Piece, Pieces):-
    findall(X-Y, (at(Board, X, Y, [Piece])), Pieces).

% verify_walk_val(+Piece2, +X-Y, +Val, -List)
verify_walk_val(Piece2, _, Val, List) :-
    Val \= Piece2, Val \= '..',
    List = [].

verify_walk_val(Piece2, X-Y, Val, List) :-
    (Val = Piece2; Val = '..'),
    List = [X-Y].

valid_walk_x1(_, _, 4-_, []).
valid_walk_x1(Board, Piece, X-Y, List) :-
    X1 is X+1,
    X1 < 5,
    goal_piece(Piece, Piece2),
    at(Board, X1, Y, [Val]),
    verify_walk_val(Piece2, X1-Y, Val, List).

valid_walk_x2(_, _, 0-_, []).
valid_walk_x2(Board, Piece, X-Y, List) :-
    X2 is X-1,
    X2 >= 0,
    goal_piece(Piece, Piece2),
    at(Board, X2, Y, [Val]),
    verify_walk_val(Piece2, X2-Y, Val, List).

valid_walk_y1(_, _, _-4, []).
valid_walk_y1(Board, Piece, X-Y, List) :-
    Y1 is Y+1,
    Y1 < 5,
    goal_piece(Piece, Piece2),
    at(Board, X, Y1, [Val]),
    verify_walk_val(Piece2, X-Y1, Val, List).

valid_walk_y2(_, _, _-0, []).
valid_walk_y2(Board, Piece, X-Y, List) :-
    Y2 is Y-1,
    Y2 >= 0,
    goal_piece(Piece, Piece2),
    at(Board, X, Y2, [Val]),
    verify_walk_val(Piece2, X-Y2, Val, List).

% valid_walk(+Board, +Piece, +Pieces, -List)
valid_walk(Board, Piece, X-Y, List) :-
    valid_walk_x1(Board, Piece, X-Y, List1),
    valid_walk_x2(Board, Piece, X-Y, List2),
    valid_walk_y1(Board, Piece, X-Y, List3),
    valid_walk_y2(Board, Piece, X-Y, List4),
    append(List1, List2, List5),
    append(List5, List3, List6),
    append(List6, List4, List).


% verify_eat_val(+Piece2, +X-Y, +Val, -List)
verify_eat_val(Piece2, _, Val, List) :-
    Val \= Piece2,
    List = [].

verify_eat_val(Piece2, X-Y, Val, List) :-
    Val = Piece2,
    List = [X-Y].

valid_eat_x1_y1(_, _, _-4, []).
valid_eat_x1_y1(_, _, 4-_, []).
valid_eat_x1_y1(Board, Piece, X-Y, List) :-
    X1 is X+1,
    X1 < 5,
    Y1 is Y+1,
    Y1 < 5,
    opposite_piece(Piece, Piece2),
    at(Board, X1, Y1, [Val]),
    verify_eat_val(Piece2, X1-Y1, Val, List).

valid_eat_x2_y2(_, _, _-0, []).
valid_eat_x2_y2(_, _, 0-_, []).
valid_eat_x2_y2(Board, Piece, X-Y, List) :-
    X2 is X-1,
    X2 >= 0,
    Y2 is Y-1,
    Y2 >= 0,
    opposite_piece(Piece, Piece2),
    at(Board, X2, Y2, [Val]),
    verify_eat_val(Piece2, X2-Y2, Val, List).

valid_eat_x2_y1(_, _, _-4, []).
valid_eat_x2_y1(_, _, 0-_, []).
valid_eat_x2_y1(Board, Piece, X-Y, List) :-
    X2 is X-1,
    X2 >= 0,
    Y1 is Y+1,
    Y1 < 5,
    opposite_piece(Piece, Piece2),
    at(Board, X2, Y1, [Val]),
    verify_eat_val(Piece2, X2-Y1, Val, List).

valid_eat_x1_y2(_, _, _-0, []).
valid_eat_x1_y2(_, _, 4-_, []).
valid_eat_x1_y2(Board, Piece, X-Y, List) :-
    X1 is X+1,
    X1 < 5,
    Y2 is Y-1,
    Y2 >= 0,
    opposite_piece(Piece, Piece2),
    at(Board, X1, Y2, [Val]),
    verify_eat_val(Piece2, X1-Y2, Val, List).

% valid_eat(+Board, +Piece, +Pieces, -List)
valid_eat(Board, Piece, X-Y, List) :-
    valid_eat_x1_y1(Board, Piece, X-Y, List1),
    valid_eat_x2_y2(Board, Piece, X-Y, List2),
    valid_eat_x2_y1(Board, Piece, X-Y, List3),
    valid_eat_x1_y2(Board, Piece, X-Y, List4),
    append(List1, List2, List5),
    append(List5, List3, List6),
    append(List6, List4, List).


% valid_moves_aux(+Board, +Piece, +Pieces, -List)
valid_moves_aux(_, _, [], []).
valid_moves_aux(Board, Piece, [X-Y|T], [[X-Y|ValidMoves]|Rest]) :-
    valid_walk(Board, Piece, X-Y, WalkMoves),
    valid_eat(Board, Piece, X-Y, EatMoves),
    append(WalkMoves, EatMoves, ValidMoves),
    valid_moves_aux(Board, Piece, T, Rest).
    

% valid_moves(+Board, +Piece, -Moves)
valid_moves(Board, Piece, Moves):-
    get_pieces(Board, Piece, Pieces),
    valid_moves_aux(Board, Piece, Pieces, Moves).

% empty_moves(+Moves)
empty_moves([]).
empty_moves([[_|ValidMoves]|Rest]) :-
    length(ValidMoves, Length),
    Length =:= 0,
    empty_moves(Rest).

% evaluate_option(+N, +Moves, -Move)
evaluate_option(_, [], _) :- fail.
evaluate_option(N, [[Piece|ValidMoves]|_], Move) :-
    length(ValidMoves, Length),
    N =< Length,
    nth1(N, ValidMoves, NMove),
    append([Piece], [NMove], Move),
    true.

% evaluate_option(+N, +Moves, -Move)
evaluate_option(N, [[_|ValidMoves]|Rest], Move) :-
    length(ValidMoves, Length),
    N > Length,
    N1 is N - Length,
    evaluate_option(N1, Rest, Move).


% commit move and update gameState
% move(+GameState, +Move, -NewGameState)
move(gameState(Board, player(Turn, Type, Piece), Player2, Turn), Move, gameState(NewBoard, player(Turn, Type, Piece), Player2, NextTurn)):-
    move_aux(Board, Type, Piece, Move, NewBoard),
	next_turn(Turn, NextTurn).
	
% move(+GameState, +Move, -NewGameState)
move(gameState(Board, Player1, player(Turn, Type, Piece), Turn), Move, gameState(NewBoard, Player1, player(Turn, Type, Piece), NextTurn)):-
    move_aux(Board, Type, Piece, Move, NewBoard),
	next_turn(Turn, NextTurn).

% move_aux(+Board, +Type, +Piece, +Move, -NewBoard)
move_aux(Board, Type, Piece, Move, NewBoard):-
    walk(Board, Piece, Move, NewBoard);
    eat(Board, Type, Piece, Move, NewBoard).

% eat(+Board, +Type, +Piece, +Move, -NewBoard)
eat(Board, Type, Piece, [X-Y, NX-NY], NewBoard):-
    opposite_piece(Piece, EnemyPiece),
	at(Board, NX, NY, [EnemyPiece]),
	replace_matrix(Board, X, Y, ['..'], BoardAux),
    replace_matrix(BoardAux, NX, NY, [Piece], NewBoardAux),
    reput_enemy_piece(NewBoardAux, Type, EnemyPiece, NewBoard).

% reput_enemy_piece(+Board, +Type, +EnemyPiece, -NewBoard)
reput_enemy_piece(Board, 'h', EnemyPiece, NewBoard):-
    display_board('h', Board),
    get_pieces(Board, '..', Spaces),
    print_reput_options(Spaces, 1),
    read_move(Option),
    digits_to_number(Option, OptionNumber),
    length(Spaces, Limit),
    (OptionNumber =< Limit ->
        nth1(OptionNumber, Spaces, X-Y),
        replace_matrix(Board, X, Y, [EnemyPiece], NewBoard)
    ;
        write('\nInvalid move!\n'),
        fail
    ).

reput_enemy_piece(Board, 'cr', EnemyPiece, NewBoard):-
    get_pieces(Board, '..', Spaces),
    random_member(X-Y, Spaces),
    replace_matrix(Board, X, Y, [EnemyPiece], NewBoard).

reput_enemy_piece(Board, 'cg', EnemyPiece, NewBoard):-
    get_pieces(Board, '..', Spaces),
    random_member(X-Y, Spaces),
    replace_matrix(Board, X, Y, [EnemyPiece], NewBoard).

% walk(+Board, +Piece, +Move, -NewBoard)
walk(Board, Piece, [X-Y, NX-NY], NewBoard):-
	goal_piece(Piece, Goal),
    (at(Board, NX, NY, ['..']);
    at(Board, NX, NY, [Goal])),
    replace_matrix(Board, X, Y, ['..'], BoardAux),
	replace_matrix(BoardAux, NX, NY, [Piece], NewBoard).

% ask for player move
% choose_move(+Board, +Player, -Move)
choose_move(Board, player(_, 'h', Piece), Move):-
    valid_moves(Board, Piece, Moves),
    print_possible_moves(Moves, 1, _),
	read_move(MoveOption),
    digits_to_number(MoveOption, MoveOptionNumber),
    evaluate_option(MoveOptionNumber, Moves, Move).
	
choose_move(Board, player(_, 'cr', Piece), Move):-
	valid_moves(Board, Piece, Moves),
	random_member([P|ValidMoves], Moves),
    ValidMoves \= [],
    random_member(M, ValidMoves),
    append([P], [M], Move).
	
choose_move(Board, player(_, 'cg', Piece), [X-Y, NX-NY]):-
	valid_moves(Board, Piece, Moves),
    evaluate_moves(Board, Piece, Moves, [], Scores),
    best_move(Scores, [_-_-_-_-100], [X-Y-NX-NY-_]).

% evaluate moves(+Board, +Piece, +Moves, +Scores, -NewScores)
evaluate_moves(_, _, [], Scores, Scores).
evaluate_moves(Board, Piece, [[P|T]|Rest], Scores, NewScores):-
    evaluate_move(Board, Piece, P, T, [], Score),
    append(Scores, Score, NextScores),
    evaluate_moves(Board, Piece, Rest, NextScores, NewScores).

% evaluate move(+Board, +Piece, +Position, +Moves, +ScoredMove, -NewScoredMove)
evaluate_move(_, _, _, [], ScoredMove, ScoredMove).
evaluate_move(Board, Piece, Position, [X-Y|T], ScoredMove, NewScoredMove) :-
    score(Board, Piece, X-Y, Score),
    append(ScoredMove, [Position-X-Y-Score], NextScoredMove),
    evaluate_move(Board, Piece, Position, T, NextScoredMove, NewScoredMove).

% score(+Board, +Piece, +NewPosition, -Score)
score(Board, Piece, NPX-NPY, Score) :-
    score_goal_distance(Board, Piece, NPX-NPY, ScoreAux),
    score_eat(Board, Piece, NPX-NPY, ScoreAux, Score).

% score_goal_distance(+Board, +Piece, +NewPosition, -Score)
score_goal_distance(Board, Piece, NPX-NPY, Score) :-
    goal_piece(Piece, Goal),
    at(Board, GX, GY, [Goal]),
    DNX is abs(3 * (GX - NPX)),
    DNY is abs(3 * (GY - NPY)),
    Score is DNX + DNY.

% score_eat(+Board, +Piece, +NewPosition, +ScoreAux, -Score)
score_eat(Board, Piece, NPX-NPY, ScoreAux, Score) :-
    opposite_piece(Piece, EnemyPiece),
    K1 is (4 - NPX) * NPX,
    K2 is (4 - NPY) * NPY,
    (at(Board, NPX, NPY, [EnemyPiece]) ->
        Score is ScoreAux - K1 - K2
    ;
        Score is ScoreAux
    ).

% retrieve best move
% best_move(+Scores, +Aux, -BestMove)
best_move([], BestMove, BestMove).
best_move([X-Y-NX-NY-Score|T], [AuxX-AuxY-AuxNX-AuxNY-AuxScore], BestMove) :-
    (Score < AuxScore -> 
        best_move(T, [X-Y-NX-NY-Score], BestMove)
    ;
        best_move(T, [AuxX-AuxY-AuxNX-AuxNY-AuxScore], BestMove)
    ).


% main gameloop 
% game_loop(+GameState)
game_loop(gameState(Board, player(_, _, P1), _, _)):-
	verify_end(Board, P1),
	format('Player ~w wins!!~n', [P1]),
    nl,
    display_board('h', Board).

game_loop(gameState(Board, player(_, _, P1), _, _)):-
	valid_moves(Board, P1, Moves),
    empty_moves(Moves),
	format('Player ~w wins!!~n', [P1]),
    nl,
    display_board('h', Board).

game_loop(gameState(Board, _, player(_, _, P2), _)):-
	verify_end(Board, P2),
	format('Player ~w wins!!~n', [P2]),
    nl,
    display_board('h', Board).
	
game_loop(gameState(Board, _, player(_, _, P2), _)):-
	valid_moves(Board, P2, Moves),
    empty_moves(Moves),
	format('Player ~w wins!!~n', [P2]),
    nl,
    display_board('h', Board).

game_loop(gameState(Board, player(Turn, Type, Piece), Player2, Turn)) :-
    repeat,
	display_game(gameState(Board, player(Turn, Type, Piece), Player2, Turn)),
	choose_move(Board, player(Turn, Type, Piece), Move),
	move(gameState(Board, player(Turn, Type, Piece), Player2, Turn), Move, NewGameState),
	game_loop(NewGameState).
	
game_loop(gameState(Board, Player1, player(Turn, Type, Piece), Turn)) :-
    repeat,
	display_game(gameState(Board, Player1, player(Turn, Type, Piece), Turn)),
	choose_move(Board, player(Turn, Type, Piece), Move),
	move(gameState(Board, Player1, player(Turn, Type, Piece), Turn), Move, NewGameState),
	game_loop(NewGameState).
