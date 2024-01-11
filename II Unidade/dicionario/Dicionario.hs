module Dicionario (Dicionario, adicionarRepeticao, novoDicionario) where

type Repeticao = Int
type Valor = String

data Dicionario = Dic Repeticao Valor

{-
- acao:       se fazer nova instância de tipos a partir do show. 
- entrada:    Dicionario (Dicionario).
- saida:      String (todos os dados concatenados);
- suposicoes: nenhuma.
- algoritmo:  Faz as concatenacoes usando o dicionario. Concatena o valor do nod e a repeticao.
-}
instance Show Dicionario where
  show (Dic repeticao valor) = valor ++ " - " ++ (show repeticao)

{-
- acao:       se fazer nova instância de tipos a partir do Eq. 
- entrada:    Dicionario (Dicionario para se fazer a impressao).
- saida:      Bool;
- suposicoes: nenhuma.
- algoritmo:  compara a igualdade dos valores da entrada.
-}
instance Eq Dicionario where
  (Dic repeticao1 valor1) == (Dic repeticao2 valor2) = valor1 == valor2

{-
- acao:       se fazer nova instância de tipos a partir do Ord. 
- entrada:    Dicionario (Dicionario para se fazer compacacoes).
- saida:      Bool;
- suposicoes: nenhuma.
- algoritmo:  compara os elementos do dicionario, usando o Ord e os valores.
-}
instance Ord Dicionario where
  (Dic repeticao1 valor1) <= (Dic repeticao2 valor2) = valor1 <= valor2
  (Dic repeticao1 valor1) >= (Dic repeticao2 valor2) = valor1 >= valor2
  (Dic repeticao1 valor1) > (Dic repeticao2 valor2) = valor1 > valor2
  (Dic repeticao1 valor1) < (Dic repeticao2 valor2) = valor1 < valor2

{-
- acao:       adicionar um na repeticao do dicionario parrasado. 
- entrada:    Dicionario.
- saida:      Dicionario (Dicionario alterado);
- suposicoes: nenhuma.
- algoritmo:  adiciona um no inteiro repeticao do dicionario passado.
-}
adicionarRepeticao :: Dicionario -> Dicionario
adicionarRepeticao (Dic repeticao valor) = (Dic (repeticao + 1) valor)

{-
- acao:       criar um novo dicionario com o valor passado. 
- entrada:    String (valor do dicionario).
- saida:      Dicionario;
- suposicoes: nenhuma.
- algoritmo:  cria o dicionario com da o valor inicial da repetica e a palavra.
-}
novoDicionario :: String -> Dicionario
novoDicionario palavra = Dic 1 palavra