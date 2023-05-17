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
        announce_invalid_move: fn(move),
    }

    StateUtil: {
        perform_move: fn(Player, Move, State, NewState),
        check_winner: fn(State, Winner)
    }

    play(State, Turn, Players, StateUtil, IOUtil, Winner)
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
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    GetMove = IOUtil.get_move,
    call(GetMove, Move),
    PerformMove = StateUtil.perform_move,
    Player = Players.human,
    AnnounceInvalid = IOUtil.announce_invalid_move,
    perform_valid_move(Turn, Player, Move, State, PerformMove, AnnounceInvalid, NewTurn, NewState),
    play(NewState, NewTurn, Players, StateUtil, IOUtil, Winner).

play(State, Turn, Players, StateUtil, IOUtil, Winner) :-
    Turn = computer,
    !,
    AnnounceState = IOUtil.announce_state,
    call(AnnounceState, State),
    GetMove = IOUtil.get_move,
    call(GetMove, Move),
    PerformMove = StateUtil.perform_move,
    Player = Players.computer,
    AnnounceInvalid = IOUtil.announce_invalid_move,
    perform_valid_move(Turn, Player, Move, State, PerformMove, AnnounceInvalid, NewTurn, NewState),
    play(NewState, NewTurn, Players, StateUtil, IOUtil, Winner).
