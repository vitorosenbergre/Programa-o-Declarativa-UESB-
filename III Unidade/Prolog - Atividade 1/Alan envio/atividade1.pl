% Base de dados

% Fatos
% Integrantes da Familia = Sara, Abraão, Isaque, Ismael, Esaú, Jacó, José

% progenitor(X,Y). /* x é progenitor de Y */
progenitor(sara,isaque).
progenitor(abraao,isaque).
progenitor(abraao,ismael).
progenitor(isaque,esau).
progenitor(isaque,jaco).
progenitor(jaco,jose).

% homem(X). /* X é um homem */
homem(abraao).
homem(isaque).
homem(ismael).
homem(esau).
homem(jaco).
homem(jose).

% mulher(X). /* X é uma mulher */
mulher(sara).

% Regras 
% conclusao :- condicoes


% mãe(X,Y) /* X é mãe de Y */
% Para se mae, X tem que gerar Y e X tem que ser mulher.
mae(X,Y):- progenitor(X,Y), mulher(X).

% pai(X,Y) /* X é pai de Y */
% Para se pai, X tem que gerar Y e X tem que ser homem.
pai(X,Y):- progenitor(X,Y), homem(X).

% filho(X,Y) /* X é filho de Y */
% Para se filho, X tem que ser gerado por Y e X tem que ser homem.
filho(X,Y):- progenitor(Y,X), homem(X).

% filha(X,Y) /* X é filha de Y */
% Para se filha, X tem que ser gerado por Y e X tem que ser mulher.
filha(X,Y):- progenitor(Y,X), mulher(X).

% irmâo(X,Y) /* X é irmão de Y */
% Para se irmao, X tem que ser filho de P e Y tem que ser filho de P,
% X tem que ser homem e X\==Y, para ele nao aceitar ele ser o proprio irmao.
irmao(X,Y):- progenitor(P,X), progenitor(P,Y), homem(X), X\==Y.

% avô(X,Y) /* X é avô de Y */
% Para ser avô, X tem que ser progenitor do pai P, 
% que eh pai de Y e X tem que ser homem. logo, X eh avô de Y.
avoo(X,Y):- progenitor(X,P), progenitor(P,Y), homem(X).

% avó(X,Y) /* X é avó de Y */
% Para ser avó, X tem que ser progenitor do pai P, 
% que eh pai de Y e X tem que ser mulher. logo, X eh avô de Y.
avoh(X,Y):- progenitor(X,P), progenitor(P,Y), mulher(X).

% bisavô(X,Y) /* X é bisavô de Y */
% Para ser bisavo, X tem que ser pai do pai de Y, e X ser homem.
% X é avô de F e F é pai de Y, logo X é bisavô de Y.
bisavo(X,Y):- avoo(X,F), progenitor(F,Y).

% tio(X,Y) /* X é tio de Y */
% Para ser tio, X tem que ser irmao do pai de Y, e X ser homem. 
% P é progenitor de Y e P é irmão de X, assim X é tio de Y
tio(X,Y) :- progenitor(P,Y),irmao(X,P).

% sobrinho(X,Y) /* X é sobrinho de Y */
% Para ser sobrinho, X tem que ser homem e Y ser tio de X. 
% X é homem e Y é tio de X, logo X é sobrinho de Y
sobrinho(X,Y) :- homem(X), tio(Y,X).


%testes
test_Pai(x) :- 
    trace, pai(Y,jose), write("O pai de José eh "), write(Y);
    trace, pai(Y,jaco), write("O pai de Jacó eh "), write(Y);
	writeln("Isaque eh pai de Ismael?"), 
    (pai(isaque,ismael) -> write("[PASS] ISAQUE EH PAI DE ISMAEL "); 
    write(" [FAIL] ISAQUE NÃO EH PAI DE ISMAEL")).

test_Mae(x) :-
    trace, mae(Y,isaque),write("A mae de isaque eh "), write(Y);
	writeln("Sara eh mae de Jacó?"),
    (mae(sara,jaco) -> write("[PASS] SARA EH MAE DE JACÓ"); 
    write(" [FAIL] SARA NAO EH MAE DE JACÓ")).

test_Filho(x) :-
    trace, filho(Y,jaco), write(Y), write(" e filho de Jacó ");
    writeln("Isaque eh filho de Esaú? "),
    (filho(isaque,esau) -> write("[PASS] ISAQUE EH FILHO DE ESAÚ"); 
    write(" [FAIL] ISAQUE NAO EH FILHO DE ESAÚ")).

test_Filha(x) :-
    nl,writeln("Sara eh filha de Abraão? "),
    (filha(sara,abraao) -> write(" [PASS] SARA EH FILHA DE ABRAÃO");
    write(" [FAIL] SARA NAO EH FILHA DE ABRAÃO")).

test_Irmao(x) :-
    trace, irmao(Y,jaco), write(Y), write(" e irmao de Jacó");
    write("Abraão eh irmao de Isaque?"), 
    (irmao(abraao,isaque) -> write("[PASS] ABRAÃO EH IRMÃO DE ISAQUE"); 
    write(" [FAIL] ABRAÃO NAO EH IRMÃO DE ISAQUE")).

test_Avoo(x) :-
    trace, avoo(abraao,Y), write("Abraão eh avô de "), write(Y);
    writeln("Isaque eh avô de Jacó? "),
    (avoo(isaque,jaco) -> write(" [PASS] ISAQUE EH AVÔ DE JACÓ"); 
    write(" [FAIL] ISAQUE NAO EH AVÔ DE JACÓ")).

test_Avoh(x) :-
    trace, avoh(sara,Y), write("Sara eh avó de "), write(Y);
    writeln("Sara eh avó de Ismael? "), 
    (avoh(isaque,jaco) -> write(" [PASS] SARA EH AVÓ DE ISMAEL"); 
    write(" [FAIL] SARA NAO EH AVÓ DE ISMAEL")).

test_Bisavo(x) :-
    trace, bisavo(abraao,Y), write("Abraão eh bisavô de "), write(Y);
    writeln("Abraão eh bisavô de Jacó? "),
    (bisavo(abraao,jaco) -> write(" [PASS] ABRAÃO EH BISAVÔ DE JACÓ"); 
    write(" [FAIL] ABRAÃO NAO EH BISAVÔ DE JACÓ ")).

test_Tio(x) :-
    trace, tio(ismael,Y), write("Ismael eh tio de "), write(Y);
    writeln("Esaú eh tio de Sara? "),
    (tio(esau,sara) -> write(" [PASS] ESAÚ EH TIO DE SARA"); 
    write(" [FAIL] ESAÚ NAO EH TIO DE SARA")).

test_Sobrinho(x) :-
    trace, sobrinho(jaco,Y), write("Jaco eh sobrinho de "), write(Y);
    writeln("Esaú eh sobrinho de Sara? "),
    (sobrinho(esau,sara) -> write(" [PASS] ESAÚ EH SOBRINHO DE SARA"); 
    write(" [FAIL] ESAÚ NAO EH SOBRINHO DE SARA")).

test_All(x) :-
    write("!! Pai !!"), test_Pai(_);
    write("!! Mãe !!"), test_Mae(_);
    write("!! Filho !!"), test_Filho(_);
    write("!! Filha !!"), test_Filha(_);
    write("!! Irmão !!"), test_Irmao(_);
    write("!! Avô !!"), test_Avoo(_);
    write("!! Avó !!"), test_Avoh(_);
    write("!! Bisavô !!"), test_Bisavo(_);
    write("!! Tio !!"), test_Tio(_);
    write("!! Sobrinho !!"), test_Sobrinho(_).