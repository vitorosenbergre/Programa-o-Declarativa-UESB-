module InterfaceDicionario where

import DicionarioMain

{-
- acao:       funcao de exibir o menu e realizar a funcao escolhida.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  vai mostrar um menu utilizando o do. Com a opcao escolhida, vai entrar no case e executar determinado comando (funcao).
- Funcoes utilizadas (do trabalho): arquivoExistente, arquivoNovo, semArquivo, main. 
-}
main :: IO()
main =
  do
    putStr (">1 Contar as palavras de um arquivo existente\n" ++
            ">2 Criar um arquivo com um texto e contar as palavras\n" ++
            ">3 Contar palavras de uma frase sem salvar em arquivo\n" ++
            ">4 Sair\n" ++
            ">")
    opcao <- getLine
    case opcao of
      "1" -> 
        do
          arquivoExistente
          main
      "2" ->
        do
          arquivoNovo
          main
      "3" ->
        do
          semArquivo
          main
      "4" -> putStrLn "Saindo..."
      _   -> main

{-
- acao:       Vai contar o arquivo com o caminho de entrada, de um arquivo jah armazenado.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  vai pedir o caminho para realizar a impressao do arquivo
- Funcoes utilizadas (do trabalho): imprimir. 
-}
arquivoExistente :: IO()
arquivoExistente =
  do
    putStr "Digite o nome do arquivo com a extensao: "
    caminho <- getLine
    texto <- readFile caminho
    writeFile ("contagem_" ++ caminho) (imprimir texto) -- cria o arquivo com a contagem

{-
- acao:       Vai criar um novo arquivo e contar as palavras.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  Pede as informacoes e, cria o arquivo com a mensagem e nome passados pelo usuario e cria o arquivo com a contagem.
- Funcoes utilizadas (do trabalho): imprimir. 
-}
arquivoNovo :: IO()
arquivoNovo =
  do
    putStr "Digite o nome do arquivo (sem extensao): "
    caminho <- getLine
    putStr "Digite a mensagem: \n> "
    mensagem <- getLine 
    writeFile (caminho ++ ".txt") mensagem                           
    writeFile ("contagem_" ++ caminho ++ ".txt") (imprimir mensagem) 

{-
- acao:       Contar palavras de uma frase sem salvar em arquivo.  
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: nenhuma. 
- algoritmo:  Pede as informacoes e faz a contagem da mensagem. Tudo sem salvar o arquivo.
- Funcoes utilizadas (do trabalho): imprimir. 
-}
semArquivo :: IO()
semArquivo =
  do
    putStr "Digite a mensagem: \n> "
    mensagem <- getLine 
    putStrLn "Contagem:"
    putStrLn (imprimir mensagem)

