:- [connect_n].
:- [game].
/% edited get move to allow usage of minimax algo
get_move(State, X) :-
    read(X).

announce_state([]).
announce_state([X | Xs]) :-
    write(X), nl, announce_state(Xs).

announce_winner(X) :-
    write("The winner is "), write(X), nl.

announce_invalid_move(Move) :-
    write("Invalid move: "), write(Move), nl.

main(Winner) :-
    form_board(6, 7, Board),
    blue_piece(Blue),
    red_piece(Red),
    Players = _{
        human: Blue,
        computer: Red
    },
    state_util(4, StateUtil),
    IOUtil = _{
        get_move: get_move,
        announce_winner: announce_winner,
        announce_state: announce_state,
        announce_invalid_move: announce_invalid_move
    },
    play(Board, human, Players, StateUtil, IOUtil, Winner).
