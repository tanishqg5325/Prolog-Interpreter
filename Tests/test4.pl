on(Item,[Item|Rest]).
on(Item,[DisregardHead|Tail]) :- on(Item,Tail).

/* Queries
    on(apples,  [pears, tomatoes, apples, grapes]).
    on(apples, [tomatoes, apples, grapes]).
*/

append([],List,List).
append([Head|Tail],List2,[Head|Result]) :- append(Tail,List2,Result).

/* Queries
    append([b,c],[one,two,three],Result).
    append([c],[one,two,three],Result).
    append([],[one,two,three],Result).
*/

delete_all([], A, []).
delete_all([Head|Tail], A, Result) :- A = Head, delete_all(Tail, A, Result).
delete_all([Head|Tail], A, [Head|Result]) :- A \= Head, delete_all(Tail, A, Result).

/* Queries
    delete_all([a,b,a,c,a,d],a,Result).
    delete_all([a,b,a,c,a,d],b,Result).
    delete_all([a,b,a,c,a,d],prolog,Result).
*/
