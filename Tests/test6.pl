edge(a, b).
edge(b, c).
edge(c, d).
edge(b, e).
% edge(e, a).
path(X, X).
path(X, Y) :- edge(X, Z), path(Z, Y).
cycle :- edge(X, Y), path(Y, X), X \= Y.
