cons(X,Y,[X|Y]).

% inverter 

inverter([],[]).
inverter([X|Y],Z) :- inverter(Y,Y1),concatenar(Y1,[X],Z).

%tamanho de uma lista

tamanho([],0).
tamanho([_|T],X) :- tamanho(T,X2), X is X2+1.

%produto de todos os elementos

produto([],1).
produto([H|T],X) :- produto(T,X2), X is X2*H.

%enesimo valor de uma lista

enesimo(1,X,[X|_]).
enesimo(N,X,[_|T]) :- N1 is N-1, enesimo(N1,X,T).

% concatenação de Listas
% concatenar(L1,L2,L3).

concatenar([],L,L).
concatenar([H|T],L,[H|L2]) :- concatenar(T,L,L2). 

%remover elemento 

remover(X,[X|T],T).
remover(X,[Y|C],[Y|D]) :- remover(X,C,D).

%ordenar uma lista

ordenar([],[]).
ordenar([H|T],LO) :- ordenar(T,TO), inserir(H,TO,LO).

inserir(E,[],[E]).
inserir(E,[H|T],[H|L]) :- E>=H, inserir(E,T,L).
inserir(E,[H|T],[E,H|T]) :- E<H.

%union(L1,L2,L3) /* L3 resulta da união de L1 com L2 */

union(L1,L2,L3) :- ordenar(L1,LO1), removerR(LO1,LP1), 
    ordenar(L2,LO2), removerR(LO2,LP2), concatenar(LP1,LP2,LPR), 
    unionAux(LPR,L3),!.

unionAux(LPR,L3):- ordenar(LPR,LPO), removerR(LPO,L3).
    
removerR([],[]).
removerR([H|[H2|T]],LR) :- H==H2, removerR([H2|T],LR).
removerR([Y|C],[Y|D]) :- removerR(C,D).

% membro da lista

membro(E,[E|_]).
membro(E,[_|T]) :- membro(E,T).

%intersection(L1,L2,L3) /* L3 resulta da intersecção de L1 com L2 */
% Ver se um elemento é membro de outra lista, se for true, só dale
%membro(E,[E|_]).
%membro(E,[_|T]) :- membro(E,T).
% ordenar

intersection(L1,L2,L3) :- removerR(L1,L1R), removerR(L2,L2R),
    intersectionAux(L1R,L2R,LI), ordenar(LI,L3),!.

intersectionAux([],_,[]).
intersectionAux([H|T],L,[H|D]) :- membro(H,L), intersectionAux(T,L,D).
intersectionAux([_|T],L,LR) :- intersectionAux(T,L,LR).

%diference(L1,L2,L3) /* L3 resulta da diferença de L1 com L2 */

diference(L1,L2,L3) :- removerR(L1,L1R), removerR(L2,L2R), 
    diferenceAux(L1R,L2R,LD), ordenar(LD,L3).

diferenceAux([],_,[]).
diferenceAux([H|T],L,LD) :- membro(H,L), diferenceAux(T,L,LD),!.
diferenceAux([H|T],L,[H|D]) :- diferenceAux(T,L,D).

%equivalence(L1,L2) /* L1 é um conjunto equivalente a L2 */

equivalence(L1,L2) :- ordenar(L1,L1O), removerR(L1O,L1R),
    ordenar(L2,L2O) ,removerR(L2O,L2R), equivalenceAux(L1R,L2R),!.

equivalenceAux([],[]).
equivalenceAux([H|T],L) :- membro(H,L), remover(H,L,LR), equivalence(T,LR).

%subset(L1,L2) /* L1 é um subconjunto de L2 */

subset(L1,L2) :- ordenar(L1,L1O), removerR(L1O,L1R),
    ordenar(L2,L2O) ,removerR(L2O,L2R), subsetAux(L1R,L2R),!.

subsetAux([],_).
subsetAux([H|T],L) :-membro(H,L), remover(H,L,LS), subset(T,LS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TESTES-CONJUNTOS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_Union(x) :- 
	trace,write("[7,4,3,5,3,1,7] U [0,1,9,9,2,4,0] = "),union([7,4,3,5,3,1,7],[0,1,9,9,2,4,0],U),write(U),nl;
	trace,write("[] U [5,4,2,1] = "),union([],[5,4,2,1],U),write(U),nl;
	trace,write("[8,3,2,0] U [] = "),union([8,3,2,0],[],U),write(U),nl;
	trace,write("[] U [] = "),union([],[],U),write(U),nl.

test_Intersection(x) :-
	trace,write("[9,8,7,6,5,4] I [4,5,6,7,8,9] = "),intersection([9,8,7,6,5,4],[4,5,6,7,8,9],I),write(I),nl;
	trace,write("[] I [2,4,8,10] = "), intersection([],[2,4,8,10],I), write(I),nl;
	trace,write("[1,3,5,9] I [] = "), intersection([1,3,5,9],[],I), write(I),nl;
	trace,write("[] I [] = "), intersection([],[],I), write(I),nl.

test_Diference(x) :- 
    trace,write("[9,7,5,3,1,0] D [0,3,7,9,2,8] = "),diference([9,7,5,3,1,0],[0,3,7,9,2,8],D),write(D),nl;
    trace,write("[] D [8,5,7,3,0] = "),diference([],[8,5,7,3,0],D),write(D),nl;
    trace,write("[1,6,3,9] D [] = "),diference([1,6,3,9],[],D),write(D),nl;
    trace,write("[] D [] = "),diference([],[],D),write(D),nl.

test_Equivalence(x) :-
   trace,write("[1,2] == [2,2,1]:"),nl,equivalence([1,2],[2,2,1]);
   trace,write("[1,2,4,1] == [2,4,1]:"),nl,equivalence([1,2,4,1],[2,4,1]);
   trace,write("[4,5,6] == [4,5,6]:"),nl,equivalence([4,5,6],[4,5,6]);
   trace,write("[] == []"),nl,equivalence([],[]);
   trace,write("[5,1,3] == [5,2,3]:"),nl,equivalence([5,1,3],[5,2,3]).

test_Subset(x) :-
   trace,write("[0,2,1,2,1] S [0,1,3,2,0,3,2] = "), subset([0,2,1,2,1],[0,1,3,2,0,3,2]),nl;
   trace,write("[0,1,2] S [1,2,0,3] = "), subset([0,1,2],[1,2,0,3]),nl;
   trace,write("[] S [2,3,7,1] = "), subset([],[2,3,7,1]),nl;
   trace,write("[9,6,3,10] S [] = "), subset([9,6,3,10],[]),nl;
   trace,write("[] S [] = "), subset([],[]),nl.
   
   
   
    
    

