:- [connect_n].
:- [game].
:- [algorithms].

get_move(X) :-
    read(X).

print_list([]).
print_list([X | Xs]) :-
    write(X), nl, print_list(Xs).
announce_state(State) :-
    [X | _] = State,
    length(X, Columns),
    Columns1 is Columns - 1,
    findall(Y, between(0, Columns1, Y), ColumnsIndicator),
    write(ColumnsIndicator), nl,
    print_list(State), nl.

announce_winner(X) :-
    write("The winner is "), write(X), nl.

announce_invalid_move(Move) :-
    write("Invalid move: "), write(Move), nl.

choose_color(1, Color, OppositeColor) :- red_piece(Color), blue_piece(OppositeColor).
choose_color(2, Color, OppositeColor) :- blue_piece(Color), red_piece(OppositeColor).

choose_algo_util(1, minimax_algo_util).
choose_algo_util(2, abp_algo_util).

choose_turn(1, human).
choose_turn(2, computer).

main(Winner) :-
    write('Rows'), nl,
    read(Rows),
    write('Columns'), nl,
    read(Columns),
    form_board(Rows, Columns, Board),
    write('Human color:'), nl, write('1. Red'), nl, write('2. Blue'), nl,
    read(ColorResponse),
    choose_color(ColorResponse, Color, OppositeColor),
    Players = _{
        human: Color,
        computer: OppositeColor
    },
    write('Consecutive pieces for winning:'), nl,
    read(ConsecutivePieces),
    state_util(ConsecutivePieces, StateUtil),
    IOUtil = _{
        get_move: get_move,
        announce_winner: announce_winner,
        announce_state: announce_state,
        announce_invalid_move: announce_invalid_move
    },
    write('Computer algorithm'), nl, write('1. Minimax'), nl, write('2. Alpha-beta pruning'), nl,
    read(AlgorithmResponse),
    choose_algo_util(AlgorithmResponse, AlgoUtilBuilder),
    write('Computer max plies look-ahead:'), nl,
    read(MaxPlies),
    call(AlgoUtilBuilder, StateUtil, MaxPlies, AlgoUtil),
    write('Does human play first?'), nl, write('1. Yes'), nl, write('2. No'), nl,
    read(TurnResponse),
    choose_turn(TurnResponse, Turn),
    play(Board, Turn, Players, StateUtil, IOUtil, AlgoUtil, Winner).
