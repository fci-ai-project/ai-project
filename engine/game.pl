:- [search].
/*
    The main loop of the game
    Ends when a winner is determined
*/

/*
    Players: {
        human: player,
        computer: player
    }

    IOUtil: {
        get_move: pred(Move),
        announce_winner: pred(Winner),
        announce_state: pred(State),
        announce_invalid_move: pred(move),
    }

    StateUtil: {
        perform_move: pred(Player, Move, State, NewState),
        check_winner: pred(State, Winner),
        switch_player: pred(Player, NewPlayer)
    }

    play(State, Turn, Players, StateUtil, IOUtil, AlgoUtil, Winner)
    Turn: human | computer
*/

change_turn(computer, human).
change_turn(human, computer).

perform_valid_move(Turn, Player, Move, State, PerformMove, _, NewTurn, NewState) :-
    call(PerformMove, Player, Move, State, NewState),
    !,
    change_turn(Turn, NewTurn).

perform_valid_move(Turn, _, Move, State, _, AnnounceInvalidMove, Turn, State) :-
    call(AnnounceInvalidMove, Move).

play(State, _, _, StateUtil, IOUtil, _, Winner) :-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner, State, Winner),
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    Announce = IOUtil.announce_winner,
    call(Announce, Winner).

play(State, Turn, Players, StateUtil, IOUtil, AlgoUtil, Winner) :-
    Turn = human,
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    GetMove = IOUtil.get_move,
    call(GetMove, Move),
    PerformMove = StateUtil.perform_move,
    Player = Players.human,
    AnnounceInvalid = IOUtil.announce_invalid_move,
    perform_valid_move(Turn, Player, Move, State, PerformMove, AnnounceInvalid, NewTurn, NewState),
    play(NewState, NewTurn, Players, StateUtil, IOUtil, AlgoUtil, Winner).

play(State, Turn, Players, StateUtil, IOUtil, AlgoUtil, Winner) :-
    Turn = computer,
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    Player = Players.computer,
    choose_best_move(AlgoUtil, StateUtil, Player, State, Move),
    PerformMove = StateUtil.perform_move,
    call(PerformMove, Player, Move, State, NewState),
    play(NewState, human, Players, StateUtil, IOUtil, AlgoUtil, Winner).
