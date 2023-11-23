:- use_module(library(lists)).
:- consult('tools.pl').

% opposite_piece(+Piece, -EnemyPiece)
opposite_piece('P2', 'P1').
opposite_piece('P1', 'P2').

% goal_piece(+Piece, -GoalPiece)
goal_piece('P2', 'G2').
goal_piece('P1', 'G1').

% init Board row
% init_row_n(+Row, -NewRow)
init_row_0(Row, NewRow) :-
	replace_list(Row, 0, ['XX'], Row1),
    replace_list(Row1, 2, ['P2'], Row2),
    replace_list(Row2, 3, ['P2'], Row3),
    replace_list(Row3, 4, ['G1'], NewRow).

init_row_1(Row, NewRow) :-
    replace_list(Row, 4, ['P2'], NewRow).

init_row_2(Row, NewRow) :-
	replace_list(Row, 0, ['P1'], Row1),
    replace_list(Row1, 4, ['P2'], NewRow).

init_row_3(Row, NewRow) :-
	replace_list(Row, 0, ['P1'], NewRow).

init_row_4(Row, NewRow) :-
	replace_list(Row, 0, ['G2'], Row1),
	replace_list(Row1, 1, ['P1'], Row2),
	replace_list(Row2, 2, ['P1'], Row3),
    replace_list(Row3, 4, ['XX'], NewRow).

% initialize board pieces
% init_pieces(+Board, -NewBoard)
init_pieces([H0, H1, H2, H3, H4], NewBoard) :-
	init_row_0(H0, B0),
    init_row_1(H1, B1),
    init_row_2(H2, B2),
    init_row_3(H3, B3),
    init_row_4(H4, B4),
    NewBoard = [B0, B1, B2, B3, B4].

% initialize board
% init_board(-NewBoard)
init_board(NewBoard) :-
	length(Row, 5),
    maplist(=(['..']), Row),
    length(Board, 5),
    maplist(=(Row), Board),
    init_pieces(Board, NewBoard).


% verify board for endGame state
% verify_end(+Board, +Piece)
verify_end([H|_], 'P1'):-
	verify_row_P1(H, 'P1').

verify_end([_|T], 'P2'):-
    append(_, [Last], T),
	verify_row_P2(Last, 'P2').

% verify rows for endGame state
% verify_row_P1(+Row, +Piece)
verify_row_P1(Row, 'P1'):-
    append(_, [Last], Row),
    Last == ['P1'].

% verify rows for endGame state
% verify_row_P2(+Row, +Piece)
verify_row_P2([H|_], 'P2'):-
    H == ['P2'].
