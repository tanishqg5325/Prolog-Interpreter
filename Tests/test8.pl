max(X, X, X).
max(X, Y, Z) :- X > Y, Z = X.
max(X, Y, Z) :- Y > X, Z = Y.

fact(0, 1).
fact(X, Y) :-
        X > 0,
        Z = X - 1,
        fact(Z, W),
        Y =  W * X.

min(X, X, X).
min(X, Y, Z) :- X > Y, Z = Y.
min(X, Y, Z) :- Y > X, Z = X.

smallest([H], H).
smallest([H|T], X) :-
    smallest(T, Y),
    min(H, Y, X).

mergesort([], []).
mergesort([A], [A]).
mergesort([A,B|R], S) :-
    split([A,B|R], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).

split([], [], []).
split([A], [A], []).
split([A,B|R], [A|Ra], [B|Rb]) :- split(R, Ra, Rb).

merge(A, [], A).
merge([], B, B).
merge([A|Ra], [B|Rb], [A|M]) :-  min(A, B, A), merge(Ra, [B|Rb], M).
merge([A|Ra], [B|Rb], [B|M]) :-  B < A, merge([A|Ra], Rb, M).

/* 
    Example Query
        mergesort([4, 7, 1, 9, 17, 3, 4, 9, 5, 2, 2], X).
*/
