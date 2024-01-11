{-
    Equipe:
        Alan Bomfim
        Breno Carvalho
        Denise Nogueira
        Eric
        Vitor Rosembergre
-}

import ParidadeCombinada

{-

Documentacao Usuario

Para testar o programa, mude a matriz de entrada para o valor desejado,
aplicando os valores corretos (uma matriz)

-}

matrizEntrada :: Matriz
matrizEntrada = [
            [0,1,0,1,0],
            [1,1,0,1,1],
            [1,0,1,0,0],
            [1,1,0,1,1],
            [1,1,1,1,0]
         ]


main :: IO() 
main = imprimeCorrecao ( corrigir matrizEntrada )

imprimeCorrecao :: Correcao -> IO()
imprimeCorrecao correcao =
    do
        putStrLn ("Mensagem:\n" ++ mensagem ++ "\n\nMatriz:\n" ++ matriz)
        where
            mensagem = snd correcao
            mudouMatriz = mensagem == "Ha um erro que foi corrigido"
            matriz = if mudouMatriz then "Mesma matriz de entrada\n\n"
                     else (imprimirMatriz (fst correcao))

