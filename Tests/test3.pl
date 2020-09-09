parent(john,paul).
parent(paul,tom).
parent(tom,mary).     
ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(X,Z), ancestor(Z,Y).
