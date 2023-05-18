/*
    AlgoUtil: {
        terminal_test: pred(State, Winner),
        utility: pred(State, MaxPlayer, Winner, Value),
        alpha_beta_violation: pred(Alpha, Beta)
    }
*/

/* Utility predicates */
max_move_and_value([Move1, Value1], [_, Value2], [Move1, Value1]) :- Value1 > Value2, !.
max_move_and_value(_, [Move2, Value2], [Move2, Value2]).
min_move_and_value([Move1, Value1], [_, Value2], [Move1, Value1]) :- Value1 < Value2, !.
min_move_and_value(_, [Move2, Value2], [Move2, Value2]).

% List received is [Move, State], Need [Move, Value]

/*
    get_max_from_list
        extract MinValue
        If MinValue is greater than the current minimum value, update the current minimum value
        if MinValue is greater than Alpha, update Alpha
        If alpha_beta_violation: the maximum value is the current maximum value
        otherwise: continue recurring to find max

    get_min_from_list
        extract MaxValue
        if MaxValue is smaller than the current minimum value, update the current minimum value
        if MaxValue is smaller than Beta, update Beta
        If alpha_beta_violation: the minimum value is the current minimum value
        otherwise: continue recurring to find min
*/

alpha_beta_violation(AlgoUtil, Alpha, Beta) :-
    AlphaBetaViolation = AlgoUtil.alpha_beta_violation,
    call(AlphaBetaViolation, Alpha, Beta).
% -----------------------------------------
decide_on_max(_, AlgoUtil, Alpha, Beta, CurrentMaximumMoveAndValue, _, CurrentMaximumMoveAndValue) :-
    alpha_beta_violation(AlgoUtil, Alpha, Beta), !.

decide_on_max(MinPred, AlgoUtil, Alpha, Beta, CurrentMaximumMoveAndValue, Xs, MoveAndValue) :-
    get_max_from_list(MinPred, AlgoUtil, Alpha, Beta, CurrentMaximumMoveAndValue, Xs, MoveAndValue).

get_max_from_list(_, _, _, _, CurrentMaximumMoveAndValue, [], CurrentMaximumMoveAndValue) :- !.

get_max_from_list(MinPred, AlgoUtil, Alpha, Beta, CurrentMaximumMoveAndValue, [X | Xs], MoveAndValue) :-
    call(MinPred, Alpha, Beta, X, MoveAndMinValue),
    max_move_and_value(MoveAndMinValue, CurrentMaximumMoveAndValue, NewCurrentMaximumMoveAndValue),
    [_, MinValue] = NewCurrentMaximumMoveAndValue,
    NewAlpha is max(Alpha, MinValue),
    decide_on_max(MinPred, AlgoUtil, NewAlpha, Beta, NewCurrentMaximumMoveAndValue, Xs, MoveAndValue).

get_max_from_list(MinPred, AlgoUtil, Alpha, Beta, List, MoveAndValue) :-
    get_max_from_list(MinPred, AlgoUtil, Alpha, Beta, [_, (-inf)], List, MoveAndValue).

% -----------------------------------------
decide_on_min(_, AlgoUtil, Alpha, Beta, CurrentMinimumMoveAndValue, _, CurrentMinimumMoveAndValue) :-
    alpha_beta_violation(AlgoUtil, Alpha, Beta), !.

decide_on_min(MaxPred, AlgoUtil, Alpha, Beta, CurrentMinimumMoveAndValue, Xs, MoveAndValue) :-
    get_min_from_list(MaxPred, AlgoUtil, Alpha, Beta, CurrentMinimumMoveAndValue, Xs, MoveAndValue).

get_min_from_list(_, _, _, _, CurrentMinimumMoveAndValue, [], CurrentMinimumMoveAndValue) :- !.

get_min_from_list(MaxPred, AlgoUtil, Alpha, Beta, CurrentMinimumMoveAndValue, [X | Xs], MoveAndValue) :-
    call(MaxPred, Alpha, Beta, X, MoveAndMaxValue),
    min_move_and_value(MoveAndMaxValue, CurrentMinimumMoveAndValue, NewCurrentMinimumMoveAndValue),
    [_, MaxValue] = NewCurrentMinimumMoveAndValue,
    NewBeta is min(Beta, MaxValue),
    decide_on_min(MaxPred, AlgoUtil, Alpha, NewBeta, NewCurrentMinimumMoveAndValue, Xs, MoveAndValue).

get_min_from_list(MaxPred, AlgoUtil, Alpha, Beta, List, MoveAndValue) :-
    get_min_from_list(MaxPred, AlgoUtil, Alpha, Beta, [_, inf], List, MoveAndValue).

% -----------------------------------------
/* Move choice predicate */
choose_best_move(AlgoUtil, StateUtil, Player, State, Move) :-
    PerformMove = StateUtil.perform_move,
    findall([M, NewState], call(PerformMove, Player, M, State, NewState), MovesWithStates),
    SwitchPlayer = StateUtil.switch_player,
    call(SwitchPlayer, Player, NewPlayer),
    get_max_from_list(
        min_value(AlgoUtil, StateUtil, Player, NewPlayer), AlgoUtil, (-inf), inf, MovesWithStates, [Move, Value]
    ),
    write([Move, Value]),nl.

utility_from_terminal(AlgoUtil, MaxPlayer, State, Value) :-
    TerminalTest = AlgoUtil.terminal_test,
    call(TerminalTest, State, Winner),
    Utility = AlgoUtil.utility,
    call(Utility, State, MaxPlayer, Winner, Value).

min_value(AlgoUtil, _, MaxPlayer, _, _, _, [Move, State], [Move, Value]) :-
    utility_from_terminal(AlgoUtil, MaxPlayer, State, Value), !.

min_value(AlgoUtil, StateUtil, MaxPlayer, Player, Alpha, Beta, [Move, State], [Move, Value]) :-
    PerformMove = StateUtil.perform_move,
    findall([M, NewState], call(PerformMove, Player, M, State, NewState), MovesWithStates),
    SwitchPlayer = StateUtil.switch_player,
    call(SwitchPlayer, Player, NewPlayer),
    get_min_from_list(
        max_value(AlgoUtil, StateUtil, MaxPlayer, NewPlayer), AlgoUtil, Alpha, Beta, MovesWithStates, [_, Value]
    ).

max_value(AlgoUtil, _, MaxPlayer, _, _, _, [Move, State], [Move, Value]) :-
    utility_from_terminal(AlgoUtil, MaxPlayer, State, Value), !.

max_value(AlgoUtil, StateUtil, MaxPlayer, Player, Alpha, Beta, [Move, State], [Move, Value]) :-
    PerformMove = StateUtil.perform_move,
    findall([M, NewState], call(PerformMove, Player, M, State, NewState), MovesWithStates),
    SwitchPlayer = StateUtil.switch_player,
    call(SwitchPlayer, Player, NewPlayer),
    get_max_from_list(
        min_value(AlgoUtil, StateUtil, MaxPlayer, NewPlayer), AlgoUtil, Alpha, Beta, MovesWithStates, [_, Value]
    ).
