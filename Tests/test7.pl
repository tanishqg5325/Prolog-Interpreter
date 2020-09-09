part(a). part(b). part(c).
red(a). black(b).
color(P, red) :- red(P),!.
color(P, black) :- black(P),!.
color(P,unknown).