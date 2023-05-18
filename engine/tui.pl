:- [connect_n].
:- [game].
:- [minimax].

get_move(X) :-
    read(X).

print_list([]).
print_list([X | Xs]) :-
    write(X), nl, print_list(Xs).
announce_state(State) :-
    print_list(State), nl.

announce_winner(X) :-
    write("The winner is "), write(X), nl.

announce_invalid_move(Move) :-
    write("Invalid move: "), write(Move), nl.

main(Winner) :-
    form_board(4, 4, Board),
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
    abp_algo_util(StateUtil, AlgoUtil),
    play(Board, human, Players, StateUtil, IOUtil, AlgoUtil, Winner).
