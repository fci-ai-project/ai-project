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
        get_move: fn(Move),
        announce_winner: fn(Winner),
        announce_state: fn(State),
    }

    StateUtil: {
        perform_move: fn(Player, Move, State, NewState),
        check_winner: fn(State, Winner)
    }

    play(State, Turn, Players, StateUtil, IOUtil, Winner)
    Turn: human | computer
*/

play(State, _, _, StateUtil, IOUtil, Winner) :-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner, State, Winner),
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    Announce = IOUtil.announce_winner,
    call(Announce, Winner).

play(State, Turn, Players, StateUtil, IOUtil, Winner) :-
    Turn = human,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    !,
    GetMove = IOUtil.get_move,
    call(GetMove, Move),
    PerformMove = StateUtil.perform_move,
    Player = Players.human,
    call(PerformMove, Player, Move, State, NewState),
    play(NewState, computer, Players, StateUtil, IOUtil, Winner).

play(State, Turn, Players, StateUtil, IOUtil, Winner) :-
    Turn = computer,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    GetMove = IOUtil.get_move,
    call(GetMove, Move),
    PerformMove = StateUtil.perform_move,
    Player = Players.computer,
    call(PerformMove, Player, Move, State, NewState),
    play(NewState, human, Players, StateUtil, IOUtil, Winner).
