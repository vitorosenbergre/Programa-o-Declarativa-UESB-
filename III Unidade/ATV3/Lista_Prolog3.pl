list([]).
list([_|_]).

member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

not_member(_, []).
not_member(X, [Y|L]) :-
    dif(X,Y),
    not_member(X, L).
 
prefix([], _).
prefix([X|P], [X|L]) :- prefix(P, L).

equal([], []).
equal([X|L1], [X|L2]) :- equal(L1, L2).

sufix([], []).
sufix([X|S], [X|L]) :- equal(S, L).
sufix(S, [_|L]) :- sufix(S, L).

sublist([], []).
sublist([X|Sub], [X|List]) :-
    prefix([X|Sub], [X|List]).
sublist([X|L1], [Y|L2]) :- dif(X,Y),sublist([X|L1], L2).

append2([], [], []).
append2([], [X|L2], List) :-
    append2([], L2, List2),
    insert(X, List2, List).
append2([X|L1], L2, List) :-
    append2(L1, L2, List2),
    insert(X, List2, List).


reverse([], []).
reverse([X|List], Rev) :- 
    reverse(List, Rev2),
    append2(Rev2, [X], Rev).

adjacent(X, Y, [X|[Y|_]]).
adjacent(X, Y, [_|[Y1|List]]) :-
    insert(Y1, List, List2),
    adjacent(X, Y, List2).

length2([], 0).
length2([_|List], N) :-
    length2(List, N1),
    N is N1+1.

first(First, [First|_]).

last(Last, [Last]).
last(Last, [_|List]) :- last(Last, List).

nth(X, 0, [X|_]).
nth(X, N, [_|List]) :- 
    N1 is N-1,
    nth(X, N1, List).

double([], []).
double([X|L], LL) :-
    double(L, LL2),
    insert(X, LL2, LL3),
    insert(X, LL3, LL).

sum([], 0).
sum([X|Xs], Num) :-
   sum(Xs, Y),
   Num is X+Y.

delete([], _, []).
delete([E|L1], E, L2) :- delete(L1, E, L2).
delete([E|L1], F, L2) :-
    dif(E,F),
    delete(L1, F, L3),
	insert(E, L3, L2).

select(_,[],[]).
select(X, [X|L1], L1).
select(X, [E|L1], L2) :-
    dif(X, E),
    select(X, L1, L3),
	insert(E, L3, L2).

insert(X, L, [X|L]).
