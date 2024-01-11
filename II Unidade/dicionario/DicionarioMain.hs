module DicionarioMain (imprimir) where

import ABB
import Dicionario

{-
- acao:       usar String como teste para o dicionario. 
- entrada:    nenhuma.
- saida:      String;
- suposicoes: nenhuma.
- algoritmo:  retorna uma String teste.
-}
teste :: String
teste = "redes aula redes aula haskell aula"

{-
- acao:       fazer a impressao do dicionario usando a String passada. 
- entrada:    String (valor da mensagem passada) .
- saida:      String;
- suposicoes: supoe-se que a expressao seja passada corretamente.
- algoritmo:  vai fazer a impressao da mensagem utilizando a funcao imprimirAuxiliar. 
- Usou a funcao word(String -> [String]) no texto e uma arvore vazia.
-}
imprimir :: String -> String
imprimir texto = imprimirAuxiliar (words texto) emptyTree

{-
- acao:       fazer a impressao da arvore do Dicionario utilizando paintTree e contagem. 
- entrada:    [String] (strings separadas da mensagem passada para impressao), ABB Dicionario (arvore vazia para passagem).
- saida:      String (string com todas as Strings  da lista concatenadas);
- suposicoes: supoe-se que a arvore esteja vazia.
- algoritmo:  vai fazer a concatenacao da mensagem utilizando a arvore. Funcoes utilizadas para isso: contagem e printTree.
-}
imprimirAuxiliar :: [String] -> ABB Dicionario -> String
imprimirAuxiliar palavras arvore = printTree (contagem palavras arvore)

{-
- acao:       fazer a contagem das palavras no dicionarios e sua repeticao, adicionando na arvore. 
- entrada:    [String] (strings separadas da mensagem passada para impressao), ABB Dicionario (arvore vazia para passagem).
- saida:      ABB Dicionario (arvore com todos os dicionarios contados).
- suposicoes: supoe-se que a arvore esteja vazia.
- algoritmo:  Se acabou a lista de Strings, retorna a arvore resultante.
- Se ainda possui Strings na lista, entao procura para ver se a palavra vai ser encontrada na arvore (se repetiu), se sim,
- entao adiciona o no com a repeticao acrescentada, diferenciando do que jah esta na arvore. Se nao estiver na arvore, 
- entao ele eh adicionado na arvore como um novoNo.
-}
contagem :: [String] -> ABB Dicionario -> ABB Dicionario
contagem [] arvore = arvore
contagem (palavra:b) arvore
  | search arvore noNovo = contagem b (insert arvore noRepetido) -- caso o valor ja esteja na arvore
  | otherwise = contagem b (insert arvore noNovo)
  where
    noNovo = novoDicionario palavra
    noRepetido = adicionarRepeticao (searchValue arvore noNovo)
