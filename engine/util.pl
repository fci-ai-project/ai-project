replace_at(0, [_ | Xs], Element, [Element | Xs]) :- !.
replace_at(N, [X | Xs], E, Ys) :-
    Np is N - 1,
    replace_at(Np, Xs, E, Zs),
    Ys = [X | Zs].


replicate(_, 0, []) :- !.
replicate(Char, Number, [Char | List]) :-
    Number1 is Number - 1,
    replicate(Char, Number1, List).

my_subset(Sub, Super) :-        
    append(_, Super2, Super),  
    append(Sub, _, Super2).