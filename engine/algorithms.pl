terminal_test(StateUtil, State, Winner) :-
    CheckWinner = StateUtil.check_winner,
    call(CheckWinner, State, Winner).

calculate_heuristic(StateUtil, Player, State, Heuristic) :-
    CalculateHeuristic = StateUtil.calculate_heuristic,
    call(CalculateHeuristic, Player, State, Heuristic).

no_alpha_beta_violation(_, _) :- false.
alpha_beta_violation(Alpha, Beta) :- Alpha >= Beta.

% utility(StateUtil, State, MaxPlayer, Winner, Value)
utility(_, _, _, tie, 0) :- !.

utility(_, _, MaxPlayer, MaxPlayer, 1000000) :- !.

utility(_, _, _, _, (-1000000)).

minimax_algo_util(StateUtil, MaxPlies, _{
    terminal_test: terminal_test(StateUtil),
    utility: utility(StateUtil),
    alpha_beta_violation: no_alpha_beta_violation,
    calculate_heuristic: calculate_heuristic(StateUtil),
    max_plies: MaxPlies
}).

abp_algo_util(StateUtil, MaxPlies, _{
    terminal_test: terminal_test(StateUtil),
    utility: utility(StateUtil),
    alpha_beta_violation: alpha_beta_violation,
    calculate_heuristic: calculate_heuristic(StateUtil),
    max_plies: MaxPlies
}).
