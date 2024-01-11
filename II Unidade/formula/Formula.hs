{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Formula (Formula, Coluna, Tabela, Linhas, montarTabelaInicial, montarTabelaFinal, verificaTautologia, pegarFormula, pegarLinha, formula) where

import FilaSR

data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Eq, Read)

{-
- acao:       se fazer nova instância de tipos a partir do show. 
- entrada:    Formula(alguma das operacoes de formula).
- saida:      String (todos os dados concatenados);
- suposicoes: nenhuma.
- algoritmo:  Faz as concatenacoes usando a formula enviada.
- Se enviada a Var, entao mostra a String.
- Se enviada a Not Formula, entao mostra a formula negada. MOSTRA.
- Se enviada a And Formula Formula, entao mostra a operacao do E/And com as formulas passadas.
- Se enviada a Or Formula Formula, entao mostra a operacao do Or/Ou com as formulas passadas.
-}
instance Show Formula where
  show(Var a) = a 
  show(Not formula) = "~(" ++ show formula ++ ")"
  show(And esquerda direita) = "(" ++ show esquerda ++ " e " ++ show direita ++ ")"
  show(Or esquerda direita) = "(" ++ show esquerda ++ " ou " ++ show direita ++ ")"

data Coluna = Col Formula Linhas deriving Show

type Linhas = [Bool]
type Tabela = [Coluna]

{-
- acao:       usar uma formula como teste. 
- entrada:    nenhuma; 
- saida:      Not Formula;
- suposicoes: nenhuma.
- algoritmo:  retorna uma Formula pronta.
-}
formula :: Formula
formula = Not (Or (Var "A") (And (Var "B") (Var "A")))

{-
- acao:       enfileirar as variaveis. 
- entrada:    Formula(a ser enfileirada) e FilaSR String (fila que armazena String); 
- saida:      FilaSR String (fila que armazena String resultante);
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  dependendo da formula de entrada, faz o enfileiramento da(s) variaveis e retorna.
-}
enfileiraVariaveis :: Formula -> FilaSR String -> FilaSR String
enfileiraVariaveis (Var a) fila = add fila a
enfileiraVariaveis (Not formula) fila = enfileiraVariaveis formula fila
enfileiraVariaveis (Or esquerda direita) fila = enfileiraVariaveis direita (enfileiraVariaveis esquerda fila)
enfileiraVariaveis (And esquerda direita) fila = enfileiraVariaveis direita (enfileiraVariaveis esquerda fila)

{-
- acao:       monta a tabela sem a ultima coluna (que calcula o resultado). 
- entrada:    Formula; 
- saida:      Tabela;
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  vai precisa da funcao montarTabelaInicialAux para formulacao da tabela.
- fila vai ter as variaveis da formula enfileiradas, enviando uma fila vazia.
- numeroLinhas vai calcular o numero de variaveis da formula.
-}
montarTabelaInicial :: Formula -> Tabela
montarTabelaInicial formula = montarTabelaInicialAuxiliar fila numeroLinhas numeroLinhas
  where
    fila = enfileiraVariaveis formula filaSRVazia -- monta a fila de variaveis
    numeroLinhas = quantidadeLinhas fila -- calcula o numero de variaveis

{-
- acao:       ajuda na montagem inicial da tabela, ou seja, sem a ultima coluna calculada. 
- entrada:    FilaSR String (fila de Strings), numeroDeLinhas (Int), intervalo (Int); 
- saida:      Tabela;
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  o algoritmo vai montando novas colunas e concatenando-as, ate o tamanho da fila for igual a 1.
- novoIntervalo calcula o valor do novo intervalo (sequencia de True ou False).
- linhas gera as linhas de uma coluna.
- novaColuna vai ser concatenada ateh gerar a tabela, que vai ser montada com o linhas e a funcao pegarPrimeiro (da fila).
- Vai fazendo o processo de retirar os elementos da fila e construir a tabela ateh acabarem o elementos da fila, retornando a nova tabela.
-}
montarTabelaInicialAuxiliar :: FilaSR String -> Int -> Int -> Tabela
montarTabelaInicialAuxiliar fila numeroLinhas intervalo
  | tamanho fila == 1 = novaColuna -- a fila so tem 1 elemento
  | otherwise = novaColuna ++ montarTabelaInicialAuxiliar (removerPrimeiro fila) numeroLinhas novoIntervalo  -- a fila tem mais de 1 elemento
  where
    novoIntervalo = div intervalo 2 -- calcula o valor do novo intervalo (sequencia de True ou False)
    linhas = gerarLinhas numeroLinhas novoIntervalo novoIntervalo True -- gera as linhas de uma coluna
    novaColuna = [Col (Var (pegarPrimeiro fila)) linhas]


montarTabelaFinal :: Tabela -> Formula -> Tabela
montarTabelaFinal tabela formula = tabela ++ montarTabelaFinalAuxiliar tabela formula

{-
- acao:       ajuda na montagem final da tabela, ou seja, a ultima coluna calculada. Retorna a tabela final. 
- entrada:    Tabela , Formula (da expressao); 
- saida:      Tabela (finalizada);
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  
- Se a Formula for "Var", vai buscar a coluna dessa variavel e retornar uma tabela com essa coluna.
- Se a Formula for "Or", ele retorna a coluna calculada das tabelas, usando:
  tabelaEsquerda vai montar a tabela utilizando a formula da esquerda, dividindo a formula primaria e passando a parte da esquerda.
  tabelaDireita vai montar a tabela utilizando a formula da direita, dividindo a formula primaria e passando a parte da direita.
  colunaEsquerda vai pegar a coluna da tabelaEsquerda com o valor necessario para resulucao da expressao.
  colunaDireita vai pegar a coluna a tabelaDireita com o valor necessario para resulucao da expressao.
  linhas vai ser a linha resultante da operacao entre a colunaEsquerda, a colunaDireita e a operacao Or.
- Se a Formula for "And", ele retorna a coluna calculada das tabelas, usando:
  tabelaEsquerda vai montar a tabela utilizando a formula da esquerda, dividindo a formula primaria e passando a parte da esquerda.
  tabelaDireita vai montar a tabela utilizando a formula da direita, dividindo a formula primaria e passando a parte da direita.
  colunaEsquerda vai pegar a coluna da tabelaEsquerda com o valor necessario para resulucao da expressao.
  colunaDireita vai pegar a coluna da tabelaDireita com o valor necessario para resulucao da expressao.
  linhas vai ser a linha resultante da operacao entre a colunaEsquerda, a colunaDireita e a operacao And.
- Se a Formula for "Not", ele vai usar a funcao map (pega todos os elementos da lista e aplica uma funcao, (a -> b) -> [a] -> [b]),
  para inverter os valores da lista linha, aplicando a operacao "Not".
  tabelaNova eh a tabela montada usando a formula.
  coluna vai pegar a coluna da tabelaNova com o valor necessario para resulucao da expressao.
  linha vai receber as linhas da coluna.
-}
montarTabelaFinalAuxiliar :: Tabela -> Formula -> Tabela
montarTabelaFinalAuxiliar tabela (Var valor) = [buscaColVar tabela (Var valor)] -- Retorna a coluna que possui a variavel
montarTabelaFinalAuxiliar tabela (Or esquerda direita) =
  [Col (Or esquerda direita) linhas]
  where
    tabelaEsqueda = montarTabelaFinalAuxiliar tabela esquerda -- Tabela
    tabelaDireita = montarTabelaFinalAuxiliar tabela direita  -- Tabela
    colunaEsquerda = head tabelaEsqueda                       -- Coluna
    colunaDireita = head tabelaDireita                        -- Coluna
    linhas = calculaLinha colunaEsquerda colunaDireita (||)   -- Linhas
montarTabelaFinalAuxiliar tabela (And esquerda direita) =
  [Col (And esquerda direita) linhas]
  where
    tabelaEsqueda = montarTabelaFinalAuxiliar tabela esquerda -- Tabela
    tabelaDireita = montarTabelaFinalAuxiliar tabela direita  -- Tabela
    colunaEsquerda = head tabelaEsqueda                       -- Coluna
    colunaDireita = head tabelaDireita                        -- Coluna
    linhas = calculaLinha colunaEsquerda colunaDireita (&&)   -- Linhas
montarTabelaFinalAuxiliar tabela (Not formula) = 
  [Col (Not formula) (map (\x -> not x) linha)]
  where
    tabelaNova = montarTabelaFinalAuxiliar tabela formula -- Tabela
    coluna = head tabelaNova -- Coluna
    linha = pegarLinha coluna

{-
- acao:       verifica a tautologia da tabela. 
- entrada:    Tabela; 
- saida:      Bool;
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  utiliza da funcao verificaTautologiaAuxiliar para fazer as verificacoes das linhas.
- ultimaColuna vai pegar o ultimo elemento da Tabela.
- linhas vai pegar a linha da ultima coluna para fazer a verificacao.
- Com as linhas, faz a verificacao da auxiliar e retorna.
-}
verificaTautologia :: Tabela -> Bool
verificaTautologia tabela = verificaTautologiaAuxiliar linhas
  where
    ultimaColuna = tabela !! (length tabela - 1) -- retorna a ultima coluna
    linhas = pegarLinha ultimaColuna

{-
- acao:       ajuda na verificacao da tautologia da tabela. 
- entrada:    Linhas; 
- saida:      Bool;
- suposicoes: supoe-se que as expressoes sejam passadas corretamente..
- algoritmo:  recebe Linhas e faz a verificacao dos seus elementos. Se ele chegar ao vazio, entao retorna True.
- Se todos os elementos forem iguais (ateh acabar os elementos), sabendo que a= Bool, entao ele vai consequentemente retornar True.
- Se um deles for diferente (false), entao retorna False.
-}
verificaTautologiaAuxiliar :: Linhas -> Bool
verificaTautologiaAuxiliar [] = True
verificaTautologiaAuxiliar (a:b)
  | a = verificaTautologiaAuxiliar b -- a = Bool
  | otherwise = False

----------------------- FUNCOES UTILITARIAS ------------------------

{-
- acao:       faz o calculo do bool de uma linha.  
- entrada:    Coluna(primeira coluna), Coluna(segunda coluna), FuncaoOperadorLogico;  
- saida:      Linhas(com as operacoes feitas); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Na linha --, se as linhas das colunas foram todas verificadas, entao vai ser o final das operacoes.
- Na linha -- para baixo, as operacoes vao ser realizadas.
- O valor vai receber o valor da operacao dos elementos 1 e 2 da respectiva linha.
- O resto vai criando a lista com todas as operacoes feitas na linha, ou seja, responsavel pela troca de linhas e armazenar os Bools da respectiva linha.
- Na linha 124 eh onde ocorre o "enlistamento" dos resultados e no "resto=" eh onda a funcao retorna o tipo Linhas.
-}
calculaLinha :: Coluna -> Coluna -> (Bool -> Bool -> Bool) -> Linhas
calculaLinha (Col formula1 []) (Col formula2 []) operador = []
calculaLinha (Col formula1 (elemento1:linhas1)) (Col formula2 (elemento2:linhas2)) operador =
  valor : resto -- inclui valor na lista gerada pela funcao resto
  where
    valor = operador elemento1 elemento2 -- Bool
    resto = calculaLinha (Col formula1 linhas1) (Col formula1 linhas2) operador -- Funcao que retorna tipo Linhas

{-
- acao:       recebe uma tabela e faz a busca da Variavel, retornando a coluna onde se encontra.  
- entrada:    Tabela(conjunto de colunas), Formula(que se deseja buscar);  
- saida:      Coluna(onde estah a formula); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Na linha --, percorreu toda a tabela e nao encontrou a variavel, enviando uma mensagem de erro
- Na linha -- ate a linha --, se a variavel da coluna for igual ao valor da variavel passada para procura, entao retorna a culona encontrada,
- se nao, entao ele continua a buca com o resto das colunas.
- Na linha --, se nao for uma variavel, entao passa essa coluna e continua para a proxima.
- Na linha --, se nao se encaixar nos outros, eh porque nao encontrou a variavel, mandando uma mensagem de error.
-}
buscaColVar :: Tabela -> Formula -> Coluna
buscaColVar [] (Var valor) = error "A variavel nao esta na tabela"
buscaColVar (Col (Var a) valores:tabela) (Var valor)
  | valor == a = Col (Var a) valores
  | otherwise = buscaColVar tabela (Var valor)
buscaColVar (coluna:tabela) (Var valor) = buscaColVar tabela (Var valor)
buscaColVar _ _ = error "A formula nao eh uma variavel"

{-
- acao:       gerar todas as linhas de uma coluna da tabela de acordo com um intervalo.  
- entrada:    quantidade (de linhas), intervalo (intervalo das linhas), incremento (outra parte do intervalo),booleano (valor booleano das linhas);  
- saida:      Coluna(onde estah a formula); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Se o intervalo for igual a quantidade, entao retorna as linhas concatenadas ateh agora.
- Se nao, vai concatenar as linhas (linhas feitas com o incremento e o valor boleano passado). Tudo isso ateh o intervalo for igual a quantidade.
-}
gerarLinhas :: Int -> Int -> Int -> Bool -> Linhas
gerarLinhas quantidade intervalo incremento booleano
  | quantidade == intervalo = linhas
  | otherwise = linhas ++ gerarLinhas quantidade (intervalo + incremento) incremento (not booleano)
    where
      linhas = gerarNLinhas incremento booleano

{-
- acao:       gera uma determinada quantidade de linhas na tabela.  
- entrada:    quantidade (de linhas), valor(valor atribuido a linha);  
- saida:      linhas(linha gerada); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Se a quantidade for igual a 0, eh porque chegou nas linhas requeridas, enlistando o vazio e retornando a Linhas.
- Enquanto a quantidade nao chegar em 0, vai adicionando o valor numa lista ate chegar a 0. Cada iteracao subtrai 1 da quantidade. 
-}
gerarNLinhas :: Int -> Bool -> Linhas
gerarNLinhas 0 valor = []
gerarNLinhas quantidade valor = valor : gerarNLinhas (quantidade - 1) valor

{-
- acao:       conta a quantidade de linhas que serao necessarias para montar a tabela.  
- entrada:    FilaSR String (Fila da contagem);  
- saida:      Int (quantidade); 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Se a fila passa for igual a uma fila vazia, entao encerrou a contagem.
- Se nao, entao vai multiplicando por 2, nas vezes do tamanho da lista. 
-}
quantidadeLinhas :: FilaSR String -> Int
quantidadeLinhas fila
  | fila == filaSRVazia = 1
  | otherwise = 2 * quantidadeLinhas (removerPrimeiro fila)

{-
- acao:       faz a impressao da tabela no terminal.  
- entrada:    Tabela;  
- saida:      String para impressao; 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  Vai passando a tabela e imprimindo seus elementos
-}
imprimirTabela :: Tabela -> String
imprimirTabela [e] = show e ++ "\n"
imprimirTabela (a:b) = show a ++ "\n" ++ imprimirTabela b

{-
- acao:       passa uma coluna e devolve as linhas.  
- entrada:    Coluna;  
- saida:      Linhas; 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  seleciona as linhas da Coluna e as retorna.
-}
pegarLinha :: Coluna -> Linhas
pegarLinha (Col formula linhas) = linhas

{-
- acao:       passa uma coluna e devolve a formula.  
- entrada:    Coluna;  
- saida:      Formula; 
- suposicoes: supoe-se que as expressoes sejam passadas corretamente. 
- algoritmo:  seleciona a Formula da Coluna e as retorna.
-}
pegarFormula :: Coluna -> Formula
pegarFormula (Col formula linhas) = formula

