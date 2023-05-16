:- use_module(library(clpfd)).
:- [util].
/*
    Board is a 2d matrix
    ####r#
    ####rb
    ####br
    etc
*/

empty_cell(#).
blue_piece(b).
red_piece(r).

form_board(0, _, []) :- !.
form_board(R, C, [X | Xs]) :-
    empty_cell(EmptyCell),
    replicate(EmptyCell, C, X),
    R1 is R - 1,
    form_board(R1, C, Xs).


get_cell(R, C, Board, Cell) :-
    nth0(R, Board, TargetRow),
    nth0(C, TargetRow, Cell).

/*
    - Start at bottom
    - Once empty cell found, insert
*/
place_piece(Piece, R, C, Board, NewBoard) :-
    get_cell(R, C, Board, Cell), % TODO: optimize to use accumulator instead of getting the cell each time
    \+(empty_cell(Cell)),
    !,
    R1 is R - 1,
    place_piece(Piece, R1, C, Board, NewBoard).

place_piece(Piece, R, C, Board, NewBoard) :-
    nth0(R, Board, TargetRow),
    replace_at(C, TargetRow, Piece, NewRow),
    replace_at(R, Board, NewRow, NewBoard).

place_piece(Piece, C, Board, NewBoard) :-
    length(Board, Length),
    R is Length - 1,
    place_piece(Piece, R, C, Board, NewBoard).

/*
    Form a \ diagonal from a starting point [R, C]
*/
diagonal(Board, [R, _], []) :-
    length(Board, Rows),
    R >= Rows, !.
diagonal([Row | _], [_, C], []) :-
    length(Row, Columns),
    C >= Columns, !.
diagonal(Board, [R, C], [D | Ds]) :-
    get_cell(R, C, Board, D),
    R1 is R + 1,
    C1 is C + 1,
    diagonal(Board, [R1, C1], Ds).

/*
    1 2 3
    4 5 6
    7 8 9

    Starting points for diagonals
    (0, 0), (0, 1), (0, 2)
    (1, 0), (2, 0)
*/
diagonals(Board, Diagonals) :-
    [H | _] = Board,
    length(H, Columns),
    Columns1 is Columns - 1,
    length(Board, Rows),
    Rows1 is Rows - 1,
    findall([0, X], between(0, Columns1, X), StartingPoints1),
    findall([X, 0], between(1, Rows1, X), StartingPoints2),
    append(StartingPoints1, StartingPoints2, StartingPoints),
    maplist(diagonal(Board), StartingPoints, NormalDiagonals),
    reverse(Board, ReversedBoard),
    maplist(diagonal(ReversedBoard), StartingPoints, AntiDiagonals),
    append(NormalDiagonals, AntiDiagonals, Diagonals).


/*
    Check connect-four winner

    For each non-empty piece in the board
    If the right of the piece is three of the same piece
    or the bottom of the piece is three of the same piece
    then this piece is a winner

    If the board is full, the it is a tie
*/

horizontal_winner([Row | _], Winner) :-
    replicate(Winner, 4, Winners),
    my_subset(Winners, Row),
    \+(empty_cell(Winner)),
    !.

horizontal_winner([_ | Board], Winner) :-
    horizontal_winner(Board, Winner).

vertical_winner(Board, Winner) :-
    transpose(Board, Transposed),
    horizontal_winner(Transposed, Winner).

diagonal_winner(Board, Winner) :-
    diagonals(Board, Diagonals),
    horizontal_winner(Diagonals, Winner).

winner(Board, Winner) :- horizontal_winner(Board, Winner), !.
winner(Board, Winner) :- vertical_winner(Board, Winner), !.
winner(Board, Winner) :- diagonal_winner(Board, Winner), !.
winner(Board, tie) :-
    \+((
        get_cell(_, _, Board, Cell),
        empty_cell(Cell)
    )).

/*
    Check vertical winner (blue)

    form_board(6, 7, Board),
    blue_piece(Blue),
    red_piece(Red),
    place_piece(Blue, 0, Board, Board1),
    place_piece(Red, 0, Board1, Board2),
    place_piece(Blue, 1, Board2, Board3),
    place_piece(Blue, 1, Board3, Board4),
    place_piece(Blue, 1, Board4, Board5),
    place_piece(Blue, 1, Board5, Board6),
    winner(Board6, Winner).
*/

/*
    Check horizontal winner (red)

    form_board(6, 7, Board),
    blue_piece(Blue),
    red_piece(Red),
    place_piece(Blue, 0, Board, Board1),
    place_piece(Red, 0, Board1, Board2),
    place_piece(Blue, 1, Board2, Board3),
    place_piece(Blue, 1, Board3, Board4),
    place_piece(Blue, 1, Board4, Board5),
    place_piece(Red, 2, Board5, Board6),
    place_piece(Red, 3, Board6, Board7),
    place_piece(Red, 4, Board7, Board8),
    place_piece(Red, 5, Board8, Board9),
    winner(Board9, Winner).
*/

/*
    Check diagonal winner (red)

    form_board(6, 7, Board),
    blue_piece(Blue),
    red_piece(Red),
    place_piece(Blue, 0, Board, Board1),
    place_piece(Red, 0, Board1, Board2),
    place_piece(Blue, 1, Board2, Board3),
    place_piece(Blue, 1, Board3, Board4),
    place_piece(Blue, 1, Board4, Board5),
    place_piece(Red, 2, Board5, Board6),
    place_piece(Red, 3, Board6, Board7),
    place_piece(Red, 4, Board7, Board8),
    place_piece(Red, 1, Board8, Board9),
    place_piece(Blue, 2, Board9, Board10),
    place_piece(Red, 2, Board10, Board11),
    place_piece(Red, 3, Board11, Board12),
    winner(Board12, Winner).
*/

/*
    Check tie

    form_board(2, 2, Board),
    blue_piece(Blue),
    red_piece(Red),
    place_piece(Blue, 0, Board, Board1),
    place_piece(Red, 0, Board1, Board2),
    place_piece(Blue, 1, Board2, Board3),
    place_piece(Red, 1, Board3, Board4),
    winner(Board4, Winner).
*/

/*
    Check invalid insert

    form_board(2, 2, Board),
    blue_piece(Blue),
    red_piece(Red),
    place_piece(Blue, 0, Board, Board1),
    place_piece(Red, 0, Board1, Board2),
    place_piece(Blue, 1, Board2, Board3),
    place_piece(Red, 1, Board3, Board4),
    place_piece(Red, 1, Board4, Board5).
*/
