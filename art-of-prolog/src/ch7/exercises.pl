% Exercise 7.5.1

no_doubles(Xs, Ys) :- no_doubles(Xs, [], Ys).
no_doubles([], Zs, []).
no_doubles([X|Xs], Zs, Ys) :-
    member(X, Zs),
    no_doubles(Xs, Zs, Ys).
no_doubles([X|Xs], Zs, [X|Ys]) :-
    no_doubles(Xs, [X|Zs], Ys).