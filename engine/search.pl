/*
    AlgoUtil: {
        terminal_test: pred(State, Winner),
        utility: pred(State, MaxPlayer, Winner, Value)
    }
*/

/* Utility predicates */
max_comparison_pred([_, Value1], [_, Value2]):- Value1 < Value2.
min_comparison_pred([_, Value1], [_, Value2]):- Value1 >= Value2.
switch_comparison_pred(max_comparison_pred, min_comparison_pred).
switch_comparison_pred(min_comparison_pred, max_comparison_pred).
min_value(AlgoUtil, StateUtil, MaxPlayer, Player, [Move, State], [Move, Value]) :-
    min_max_value(AlgoUtil, StateUtil, min_comparison_pred, MaxPlayer, Player, [Move, State], [Move, Value]).

/* Move choice predicate */
choose_best_move(AlgoUtil, StateUtil, Player, State, Move) :-
    PerformMove = StateUtil.perform_move,
    findall([A, NewState], call(PerformMove, Player, A, State, NewState), StatesWithMoves),
    SwitchPlayer = StateUtil.switch_player,
    call(SwitchPlayer, Player, NewPlayer),
    maplist(min_value(AlgoUtil, StateUtil, Player, NewPlayer), StatesWithMoves, MinValuesWithMoves),
    max_member(max_comparison_pred, [Move, _], MinValuesWithMoves).

/* Generic min_max_value instead of min_value and max_value */
min_max_value(AlgoUtil, _, _, MaxPlayer, _, [Move, State], [Move, Value]) :-
    TerminalTest = AlgoUtil.terminal_test,
    call(TerminalTest, State, Winner),
    !,
    Utility = AlgoUtil.utility,
    call(Utility, State, MaxPlayer, Winner, Value).

min_max_value(AlgoUtil, StateUtil, ComparisonPred, MaxPlayer, Player, [Move, State], [Move, Value]) :-
    PerformMove = StateUtil.perform_move,
    findall([A, NewState], call(PerformMove, Player, A, State, NewState), StatesWithMoves),
    switch_comparison_pred(ComparisonPred, NewComparisonPred),
    SwitchPlayer = StateUtil.switch_player,
    call(SwitchPlayer, Player, NewPlayer),
    maplist(min_max_value(AlgoUtil, StateUtil, NewComparisonPred, MaxPlayer, NewPlayer), StatesWithMoves, ValuesWithMoves),
    max_member(ComparisonPred, [_, Value], ValuesWithMoves).
