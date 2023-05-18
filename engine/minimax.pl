terminal_test(StateUtil, State, Winner) :-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner, State, Winner).

% utility(StateUtil, State, MaxPlayer, Winner, Value)
utility(_, _, _, tie, 0) :- !.

utility(_, _, MaxPlayer, MaxPlayer, 1) :- !.

utility(_, _, _, _, (-1)).

minimax_algo_util(StateUtil, _{
    terminal_test: terminal_test(StateUtil),
    utility: utility(StateUtil)
}).
