% Base de dados

% Fatos
% FUNCIONARIO(MATRICULA,NOME,SALARIO,DATA_DE_ADMISSAO).
dataDeAdmissao(dia,mes,ano).

funcionario(201912192,vitor,2000,dataDeAdmissao(05,10,2019)).
funcionario(201711873,pedro,1000,dataDeAdmissao(04,10,2017)).
funcionario(201911543,vinicius,1500,dataDeAdmissao(06,10,2015)).
funcionario(201512721,alvaro,900,dataDeAdmissao(03,10,2019)).
funcionario(201811982,clementina,3000,dataDeAdmissao(07,10,2018)).
funcionario(201412548,claudia,2400,dataDeAdmissao(08,10,2014)).
funcionario(201711275,gustavo,1200,dataDeAdmissao(02,10,2017)).
funcionario(201912432,leonardo,2700,dataDeAdmissao(09,10,2019)).
funcionario(201912375,alessandro,500,dataDeAdmissao(10,10,2019)).
funcionario(201911311,erica,1300,dataDeAdmissao(11,10,2019)).

% Regras 
% conclusao :- condicoes
% a questao vai pegar todos os funcionarios, verificar qual deles possue o salario 
% entre 1000 e 2000 e vai imprimir o nome daqueles que estiverem dentro das condições.
questao01a(x) :- funcionario(_,Nome,Salario,_), Salario >= 1000, Salario =<2000, 
    write("Nome: "),write(Nome),nl.

% a questao utiliza dos dados do funcionario, para verificar se o ano é igual ou maior que 2018 e
% se o salario ganho é maior que 2000. Se isso for verdadeiro, entao imprime o nome e a matricula da pessoa.
questao01b(x) :- funcionario(Matricula,Nome,Salario,dataDeAdmissao(_,_,Ano)),
    Ano >= 2018, Salario >2000, write("Matricula: "), write(Matricula), nl,
    write("Nome: "),  write(Nome),nl.

%regra que testa a primeira questao
test_1(x) :- 
    trace, questao01a(x);
    trace, questao01b(x).

    
	



