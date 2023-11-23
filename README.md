# **Claustro**

## **Installation and Execution**

Other than Sicstus, **no other software** is required to execute the game.
To run the program, you should follow these steps:

1. Open Sicstus.
2. Consult `main.pl`, located on the `src` folder.
3. Call the `play` predicate without any arguments.

------------------

## **Game Description**

- Claustro is an abstract two-player game with deceptively simple rules: goal, movement, and capture, akin to many others in its genre. However, what sets Claustro apart is its innovative capture mechanism. In this game, captured pieces aren't eliminated; instead, they must be strategically placed back on the board by the capturing player. This subtle twist adds layers of complexity, making Claustro both challenging and enthralling.
- To play, you'll need a 5 by 5 grid and two sets of differently colored pawns. Arrange the grid diagonally between the players, with two corners serving as each player's respective goals. The other two corners are non-occupiable squares. At the start, each player places their four pawns on the squares adjacent to their closest corner. Players take turns moving a pawn orthogonally towards the goal or capturing an opponent's pawn diagonally in any of the four directions. Captured pieces must be immediately reintroduced onto any unoccupied square by the capturing player before the turn ends.
- The objective? Be the first to maneuver a pawn into the goal to claim victory. Claustro's elegant simplicity, combined with its captivating twist on capture mechanics, makes it a game that's easy to craft and endlessly challenging to master.

  ### **Rules**

1. The board is initialized with all pieces.
2. Playes take turns moving their pieces.
3. You can only move orthogonally, except when there is a piece of the oposite player diagonally to your piece. When this happens you eat the piece, your piece moves and you choose a spot to relocate the opponent's piece.
4. You cannot move into the corners named XX.
5. The winner is decided whenever one of the players manages to reach the oposite corner.
6. If a player have no possible moves, this player instantly wins.

Official Site - [Claustro Game](https://boardgamegeek.com/boardgame/391334/claustro)

------------------

## **Game Logic Implementation**

The `src` folder contains different files, each of them used for different purposes:
	
|   File   | Description |
|   :--:   |:--|
|`board.pl`|Takes care of board-related validations| 
|`game.pl` |Contains predicates to enforce the game rules| 
|`io.pl`   |Holds the role to output the game results to the console and reading user input| 
|`main.pl` |Simply contains the `play` predicate| 
|`menu.pl` |Reponsible for handling the user input and mapping to the correct menu option| 
|`tools.pl`|Utility predicates| 

------------------

## **Internal State Representation**

|Field|Description|
|:--:|:--|
|Board      |Matrix where each element represents the players pieces|
|Player 1   |This structure is composed by the player's id(1), the player's type(human, greedy robot, random robot), the player's piece|
|Player 2   |This structure is composed by the player's id(2), the player's type(human, greedy robot, random robot), the player's piece|
|Player turn|1 or 2 to define which player is playing. Shifts after each move|


The `gameState` data structure is initialized like this:
```Prolog
init_game(Type1, Type2, gameState(NewBoard, player(1, Type1, 'P1'), player(2, Type2, 'P2'), 1)) :-
	init_board(NewBoard).
```
In the `init_game` predicate, the `Type1` and the `Type2` indicate, respectively, the type of Player0 and Player1. Each player gets initialized with 4 pieces, his type and piece representation. The `init_board` predicate outputs the `NewBoard` variable, initializing the board.

These are examples of the game state through the game
- Initial Game State

```	Prolog
gameState(NewBoard, player(1, 'h', 'P1'), player(2, 'h', 'P2'), 1))
```
New Board:

```Prolog
[XX] [..] [P2] [P2] [G1]
[..] [..] [..] [..] [P2]
[P1] [..] [..] [..] [P2]
[P1] [..] [..] [..] [..]
[G2] [P1] [P1] [..] [XX]
```
- Mid Game State

```	Prolog
gameState(NewBoard, player(1, 'cr', 'P1'), player(2, 'cg', 'P2'), 2))
```
MidGameBoard:
```Prolog
[XX] [P2] [..] [P2] [G1]
[..] [..] [P2] [..] [..]
[P1] [P1] [P1] [P2] [..]
[..] [..] [..] [..] [..]
[G2] [..] [P1] [..] [XX]
```

- End Game State

```	Prolog
gameState(NewBoard, player(1, 'h', 'P1'), player(2, 'cg', 'P2'), 1))
```
EndGameBoard:

```Prolog
[XX] [P2] [..] [P2] [P1]
[P1] [..] [P2] [..] [..]
[..] [P1] [..] [..] [..]
[..] [P2] [..] [..] [..]
[G2] [P1] [..] [..] [XX]
```

------------------

### **Game State Visualization**

The game has a square board that is printed before every player move. The board is represented using a 5x5 grid. This is the board at the beginning of the game.

```Prolog           
[XX] [..] [BB] [BB] [WR]  
[..] [..] [..] [..] [BB]
[RR] [..] [..] [..] [BB]
[RR] [..] [..] [..] [..]
[WB] [RR] [RR] [..] [XX]
```
Here is how we represent the board:

The empty cells are filled with '..' in order for a more organized structure.

```Prolog
init_board(NewBoard) :-
	length(Row, 5),
    maplist(=(['..']), Row),
    length(Board, 5),
    maplist(=(Row), Board),
    init_pieces(Board, NewBoard).
	
init_pieces([H0, H1, H2, H3, H4], NewBoard) :-
	init_row_0(H0, B0),
    init_row_1(H1, B1),
    init_row_2(H2, B2),
    init_row_3(H3, B3),
    init_row_4(H4, B4),
    NewBoard = [B0, B1, B2, B3, B4].
```

The game is displayed through the following code:

```Prolog
display_game(gameState(Board, player(Turn, Type , Piece), _, Turn)):-
	format('~nTurn of piece ~w~n', [Piece]),
	nl,
	length(Board, Size),
	display_move_direction(Size),
	display_board(Type, Board).
```

The `display_move_direction(+Size)` predicate gives information about the board.

In the `display_board(+Type, +Board)` predicate, if a computer type player makes a move, the game asks for input(pressing any key) before continuing, to assure the user sees the board before the next move.

There are also display predicates for the menu, such as:

- `print_title`: prints the title of the game in the menu;
- `print_options`: prints different game modes in the menu.

The input reading predicates:

- `read_keyboard(-Input)`:  general input reading function;
- `read_move(-MoveOption)`: ask user for move and validate input;
- `read_option(-Option)`: ask user for menu option;

------------------

### **Game Plays Execution**

The game plays are acquired through the `choose_move(+Board, +Player, -Move)` predicate which behaves differently depending on the type of the player, human, cg or cr. In all cases, the predicate generates the valid moves. The move that gets picked depends on the player Type. If the play is made by the greedy bot, then a greedy move is selected from the list. On the other hand, the random bot selects a random move from the list. The human player is showed a list of possible moves and is asked for imput about what option he intends to play.

Each play is validated by applying the following logic:

The predicate `valid_moves(+Board, +Piece, -Moves)`, receives the current board and outputs the list of valid moves.

The move predicate was implemented as follows:

```Prolog
move(gameState(Board, player(Turn, Type, Piece), Player2, Turn), Move, gameState(NewBoard, player(Turn, Type, Piece), Player2, NextTurn)):-
    move_aux(Board, Piece, Move, NewBoard),
	next_turn(Turn, NextTurn).
```
Where `move_aux(+Board, +Piece, +Move, -NewBoard)` predicate is declared as:
```Prolog
move_aux(Board, Piece, Move, NewBoard):-
    walk(Board, Piece, Move, NewBoard);
    eat(Board, Piece, Move, NewBoard).
```
The `walk(Board, Piece, Move, NewBoard);` predicate is responsible for the orthogonal options. The `eat(Board, Piece, Move, NewBoard).` predicate is responsible for the diagonal options

------------------

### **Valid Plays**

Firstly, a list of all possible moves is printed asking the player what option would they rather choose.

The `valid_moves(+Board, +Piece, -Moves)` predicate was implemented as follows:

```Prolog
valid_moves(Board, Piece, Moves):-
    get_pieces(Board, Piece, Pieces),
    valid_moves_aux(Board, Piece, Pieces, Moves).%, write('Moves = '), print(Moves), nl.
```

Where the `valid_moves_aux(+Board, +Piece, +Pieces, -List)` predicate was implemented like this:

```Prolog
valid_moves_aux(_, _, [], []).
valid_moves_aux(Board, Piece, [X-Y|T], [[X-Y|ValidMoves]|Rest]) :-
    valid_walk(Board, Piece, X-Y, WalkMoves),
    valid_eat(Board, Piece, X-Y, EatMoves),
    append(WalkMoves, EatMoves, ValidMoves),
    valid_moves_aux(Board, Piece, T, Rest).
```
Through the `valid_walk(+Board, +Piece, +Pieces, -List)` and the `valid_eat(+Board, +Piece, +Pieces, -List)` predicates we check all the possible walk moves and all the possible eat moves, respectively.

------------------

### **Endgame**

The game ends either when of the players manages to reach the oposite corner, or one of the players have no possible moves left.

```Prolog
verify_end([H|_], 'RR'):-
	verify_row_P1(H, 'RR').
```
```Prolog
verify_end([_|T], 'BB'):-
    append(_, [Last], T),
	verify_row_P2(Last, 'BB').
```
The `verify_row_P1(H, 'RR').`and `verify_row_P2(Last, 'BB').`both chech if the user reached the goal.

We also check if the player has no possible moves, in case this happens the player with no moves wins the game.

------------------

### **Computer Plays**

Depending on the level of the computer, we use different strategies to generate automated plays.

- random computer

Simply generate all possible moves using `valid_moves` predicate and select a random move from the list using `random_member` predicate imported from the `random` library. 

- greedy computer

All moves are caluclated and assigned a score considering the distance the piece is to the goal after the move is made. Through the following predcicate we calculate the scores:

```Prolog
score(Board, Piece, NPX-NPY, Score) :-
    score_goal_distance(Board, Piece, NPX-NPY, ScoreAux),
    score_eat(Board, Piece, NPX-NPY, ScoreAux, Score).
```

------------------

## **Conclusions**

Completing this project allowed us to develop our prolog skills as well as understand more about this language. One of the aspects that with more time we could improve is the "Greedy" type of player, which in our case, his only objective is to reach the victory square as quickly as possible, but on certain occasions, it could be smarter to eat a enemy piece...

------------------

## **Bibliography**

- [Claustro Game](https://boardgamegeek.com/boardgame/391334/claustro)
- [Rulesbook](https://boardgamegeek.com/filepage/260778/rules)

------------------

## **Group Identification**

<table>
	<thead>
		<tr>
			<th>Group</th>
			<th>Number</th>
			<th>Name</th>
			<th>Contribution</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td rowspan=3>Claustro_7</td>
			<td>202005083</td>
			<td>Nelson Teixeira Campos</td>
			<td>40%</td>
		</tr>
		<tr>
			<td>202108679</td>
			<td>Rodrigo Moisés Baptista Ribeiro</td>
			<td>30%</td>
		</tr>
				<tr>
			<td>202103345</td>
			<td>Rui Pedro Monteiro Reis Magalhães Teixeira</td>
			<td>30%</td>
		</tr>
	</tbody>
</table>
