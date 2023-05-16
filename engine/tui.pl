:- [connect_four].
:- [game].

get_move(X) :-
    read(X).

announce_state([]).
announce_state([X | Xs]) :-
    write(X), nl, announce_state(Xs).

announce_winner(X) :-
    write("The winner is "), write(X), nl.

main(Winner) :-
    form_board(6, 7, Board),
    blue_piece(Blue),
    red_piece(Red),
    Players = _{
        human: Blue,
        computer: Red
    },
    StateUtil = _{
        perform_move: place_piece,
        check_winner: winner
    },
    IOUtil = _{
        get_move: get_move,
        announce_winner: announce_winner,
        announce_state: announce_state
    },
    play(Board, human, Players, StateUtil, IOUtil, Winner).
