{-
    Equipe:
        Alan Bomfim
        Breno Carvalho
        Denise Nogueira
        Eric
        Vitor Rosembergre
-}

-----------------------------------------------------------------------
-- Use runghc Testes.hs para executar
-----------------------------------------------------------------------
module Testes where

import HTest
import ParidadeCombinada

--- Eh possivel tambem exeutar a funcao main
main :: IO()
main = 
  do
    putStrLn (imprimirMatriz matrizSemErro)
    putStrLn ("Processando...")
    imprimir matrizSemErro
    imprimir matrizUmErro
    imprimir matrizDoisErros
    imprimir matrizErroQuadrado

imprimir :: Matriz -> IO()
imprimir matriz= 
  do
    let tupla = corrigir matriz
    putStrLn ("Matriz antes:\n" ++ (imprimirMatriz matriz) ++ (snd tupla) ++ "\n" ++ imprimirMatriz (fst tupla))
  --where
    --(matriz, resultado) = corrigir matriz

matrizSemErro :: Matriz
matrizSemErro = [
            [0,1,0,1,0],
            [1,1,0,1,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ]

resultadoCorretoMatrizSemErro :: Correcao
resultadoCorretoMatrizSemErro = (matrizSemErro, "Nao ha erro")

--erro na linha 2 e coluna 4
matrizUmErro :: Matriz
matrizUmErro = [
            [0,1,0,1,0],
            [1,1,0,0,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ]

resultadoCorretoMatrizUmErro :: Correcao
resultadoCorretoMatrizUmErro = ([
            [0,1,0,1,0],
            [1,1,0,1,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ], "Ha um erro que foi corrigido")

--erro na linha 2, colunas 2 e 4
matrizDoisErros :: Matriz
matrizDoisErros = [
            [0,1,0,1,0],
            [1,0,0,0,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ]

resultadoCorretoMatrizDoisErros :: Correcao
resultadoCorretoMatrizDoisErros = ([
            [0,1,0,1,0],
            [1,0,0,0,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ], "Ha erro, mas nao pode ser corrigido")

--erros na linha 1 coluna 1, linha 1 coluna 4, linha 3 coluna 1 e linha 3 coluna 4
matrizErroQuadrado :: Matriz
matrizErroQuadrado = [
            [1,1,0,0,0],
            [1,1,0,1,1],
            [0,0,1,1,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ]

resultadoCorretoMatrizErroQuadrado :: Correcao
resultadoCorretoMatrizErroQuadrado = ([
            [1,1,0,0,0],
            [1,1,0,1,1],
            [0,0,1,1,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ], "Nao ha erro")


---------- Testes -----------
testCorrigir :: IO()
testCorrigir = 
  do
    assertEqual (corrigir matrizSemErro)      resultadoCorretoMatrizSemErro
    assertEqual (corrigir matrizUmErro)       resultadoCorretoMatrizUmErro
    assertEqual (corrigir matrizDoisErros)    resultadoCorretoMatrizDoisErros
    assertEqual (corrigir matrizErroQuadrado) resultadoCorretoMatrizErroQuadrado