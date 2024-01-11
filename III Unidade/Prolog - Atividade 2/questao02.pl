% Fatos

% representacao do grafo atraves de suas arestas (estradas nesse caso)
% cada estrada tem uma cidade de origem, uma de chegada e a distancia entre elas
% estrada(Origem Destino, Km)
estrada(a,d,23).
estrada(a,b,25).
estrada(b,c,19).
estrada(b,e,32).
estrada(c,d,14).
estrada(c,f,28).
estrada(d,f,30).
estrada(e,f,26).

% Regras

% dist eh a relação transitiva que calcula a distancia entre uma cidade A e uma B
% caso base (de A se chega diretamente na em B, são adjacentes)
dist(A,B,D) :- estrada(A,B,D).
% caso geral da cidade A se chega na T com uma distancia D1 e de T calculamos
% a distancia D2 para B, ao final calculamos a distancia D como a soma das
% duas outras distancias
dist(A,B,D) :- estrada(A,T,D1), dist(T,B,D2), D is D1 + D2.

% teste
teste(X) :- 
    trace, estrada(a,d,23), estrada(b,c,D), write(D), estrada(b,e,32);
    trace, dist(b,d,D), write(D), dist(b,a,D1).