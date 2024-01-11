{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InterfaceFormula (main, montarTabelaCompleta, imprimirTabela, tautologia) where

import Formula

  {-
  - acao:       funcao que vai exibir o menu e realizar a funcao escolhida com ajuda da funcao mainAuxiliar.  
  - entrada:    acao de IO() (input/output).  
  - saida:      passa uma formula vazia. 
  - suposicoes: a suposicao esteja vazia. 
  - algoritmo:  vai chamar a funcao mainAuxiliar para iniciar o menu, mandando uma formula vazia. 
  -}
main :: IO()
main = mainAuxiliar []

{-
- acao:       funcao de exibir o menu e realizar a funcao escolhida.  
- entrada:    Formula.  
- saida:      acao de IO() (input/output).  
- suposicoes: nenhuma. 
- algoritmo:  vai mostrar um menu utilizando o do. Com a opcao escolhida, vai entrar no case e executar determinado comando (funcao)
  utilizando as funcoes feitas para impressao e manipulacao da tabela.
  Funcoes utilizadas (do trabalho): montarTabelaCompleta, imprimirTabela, tautologia, imprimirLista, montarTabelaCompleta.
-}
mainAuxiliar :: [Formula] -> IO()
mainAuxiliar formulas =
  do
    putStr (">1 Entrar com uma formula e montar tabela\n" ++
            ">2 Montar tabela de uma formula do historico (historico eh apagado ao fim da execucao)\n" ++
            ">3 Sair\n" ++
            ">")
    opcao <- getLine
    case opcao of
      "1" ->
        do
          putStrLn instrucoes
          putStr "Formula: "
          formulaString <- getLine
          let formula = (read formulaString :: Formula)
          --imprimindo a formula
          tabela <- montarTabelaCompleta formula
          tabelaString <- imprimirTabela tabela
          putStrLn tabelaString
          ehTautologia <- tautologia tabela
          putStrLn ehTautologia
          --inclui a nova formula na lista e chama a funcao mainAuxiliar
          mainAuxiliar (formula : formulas)
      "2" ->
        do
          if null formulas then
            do
              putStrLn "Historico vazio"
              mainAuxiliar formulas
          else
            do
              putStrLn "Escolha uma das formulas"
              putStrLn (imprimirLista formulas 0)
              putStr ">"
              posicaoString <- getLine
              let posicao = read posicaoString
              if posicao > (length formulas -1) || posicao<0 then
                do
                  putStrLn "Posicao invalida"
                  mainAuxiliar formulas
              else
                do
                  --imprimindo a formula
                  tabela <- montarTabelaCompleta (formulas !! posicao)
                  tabelaString <- imprimirTabela tabela
                  putStrLn tabelaString
                  ehTautologia <- tautologia tabela
                  putStrLn ehTautologia
                  --chama a funcao mainAuxiliar passando a formulas
                  mainAuxiliar formulas
      "3" -> putStrLn "Saindo..."
      _   ->
        do
          putStrLn "Opcao invalida, tente novamente"
          mainAuxiliar formulas

{-
- acao:       recebe uma lista e retorna uma String para impressao, padronizada.  
- entrada:    Lista, contador (Int).  
- saida:      String (organizada para quando impressa estar padronizada); 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  Typeclass Show, onde valores devem ser membros de Show.
- Se houver apenas um elemento na lista, faz a ultima concatenacao da Lista.
- Se nao houver apenas um elemento, entao faz a concatenacao com seu primeiro elemento e continua concatenando com os outros da lista.
-}
imprimirLista :: Show a => [a] -> Int -> String
imprimirLista [e] cont =  show cont ++ ". " ++ show e
imprimirLista (a:b) cont = show cont ++ ". " ++ show a ++ "\n" ++ imprimirLista b (cont+1)

{-
- acao:       usar como String de Instrucao do programa. 
- entrada:    nenhuma.
- saida:      String (com as instruicoes);
- suposicoes: nenhuma.
- algoritmo:  retorna uma string padronizada com as instrucoes.
-}
instrucoes :: String
instrucoes = "As formulas sao montadas da seguinte forma:\n" ++
              "Var String :: Formula\n" ++
              "Not Formula :: Formula\n" ++
              "And Formula Formula :: Formula\n" ++
              "Or Formula Formula :: Formula\n" ++
              "Exemplo: Not (Or (Var \"A\") (And (Var \"B\") (Var \"A\")))\n"

{-
- acao:       faz a montagem da tabelaFinal recebendo a formula.  
- entrada:    Formula.  
- saida:      IO Tabela (tabela montada); 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  recebe uma formula e faz a montagem da tabela, utilizando as funcoes montarTabelaInicial e montarTabelaFinal.
- Retorna a tabela completa.
-}
montarTabelaCompleta :: Formula -> IO Tabela
montarTabelaCompleta formula =
  do
    let tabela = montarTabelaInicial formula -- inicio da tabela, somente com as variaveis
    let tabelaFinal = montarTabelaFinal tabela formula
    return tabelaFinal

{-
- acao:       faz a tautologia da tabela final utilizando a funcao verificaTautologia e retorna se eh ou nao tautologia.  
- entrada:    Tabela (Final).  
- saida:      IO String (String com mensagem); 
- suposicoes: supoe-se que as parametros foram passados corretamente. 
- algoritmo:  usa a funcao verificaTautologia. Vai retornar uma mensagem indicando se eh uma tautologia.
-}
tautologia :: Tabela -> IO String
tautologia tabela =
  do
    let ehTautologia = verificaTautologia tabela -- recebe true se for tautologia
    if ehTautologia then
      return "Eh tautologia"
    else
      return "Nao eh tautologia"

------------------- FUNCOES PARA IMPRIMIR A TABELA -------------------------

{-
- acao:       recebe uma tabela e faz a impressao. 
- entrada:    Tabela.  
- saida:      IO String (String recebida). 
- suposicoes: nenhuma. 
- algoritmo:  recebe uma mensagem com o requerimento da entrada (usando getLine para obter);
-}
imprimirTabela :: Tabela -> IO String
imprimirTabela tabela =
  do
    let matriz = montarMatriz tabela -- [[String]]
    let texto = transformaEmTabela matriz 1 (length matriz)
    return texto

{-
- acao:       recebe uma tabela e monta uma matriz com ela. 
- entrada:    Tabela.  
- saida:      [[String]] (Matriz de String). 
- suposicoes: nenhuma. 
- algoritmo:  vai pegar as colunas da tabela e montar uma matriz com suas colunas. 
- Utiliza a funcao montarColuna como auxiliar.
-}
montarMatriz :: Tabela -> [[String]]
montarMatriz b
  = map
      (\ a -> [show (pegarFormula a)] ++ montarColuna (pegarLinha a)) b

{-
- acao:       recebe uma linha e monta uma uma lista com os elementos. 
- entrada:    Linhas.  
- saida:      [String] (Lista de String). 
- suposicoes: nenhuma. 
- algoritmo:  Se a linha estiver vazia, entao retorna uma lista vazia, que esta conectando com os outros valores (em String).
- Se a for Verdadeiro entao concatena a String "V" com a String final. 
- Se for Falso entao concatena a String "F" com a String final.
-}
montarColuna :: Linhas -> [String]
montarColuna [] = []
montarColuna (a:b)
  | a = "V" : montarColuna b
  | otherwise = "F" : montarColuna b

{-
- acao:       Vai receber uma matriz de Strings (que vai ser a tabela) em uma String representando a tabela. 
- entrada:    [[String]] (Matriz de String), contador (Int), quantidadeColunas (Int).  
- saida:      String (representa a tabela). 
- suposicoes: nenhuma. 
- algoritmo:  Se o contador chegar na quantidadeDeColunas passada, ele faz a concatenacao na String,
  zera o contador, e passa para a outra lista.
  Se nao chegar na quantidade de colunas, entao concatena os elementos da linha, e continua o processo aumentando o contador.
  Se nao for nenhuma das alternativas, entao jah concatenou todos os elementos da matriz. 
-}
transformaEmTabela :: [[String]] -> Int -> Int -> String 
transformaEmTabela ((a:b):c) cont quantidadeColunas 
  | cont == quantidadeColunas = a ++ "\n" ++ transformaEmTabela (c ++ [b]) 1 quantidadeColunas
  | otherwise = a ++ "    " ++ transformaEmTabela (c ++ [b]) (cont + 1) quantidadeColunas
transformaEmTabela _ _ _ = ""