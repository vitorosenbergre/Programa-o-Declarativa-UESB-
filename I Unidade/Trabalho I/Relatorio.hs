---------------------------------------------------------
-- Relatorio.hs
-- Para executar digitar: runghc Relatorio.hs

---------------------------------------------------------

{-
INTEGRANTES DA EQUIPE
Nome da Equipe: hmm cafeh com banana!! (ou tv de tudo quebrada)
Personagens:
  - Alan Bonfim
  - Breno Carvalho
  - Denise Nogueira
  - Eric Vinicius
  - Vitor Rosembergre
-}

module Relatorio where

main :: IO()
main = do
        putStr menuPrincipal
        op <- getLine
        imprimirRelatorio (read op)

imprimirRelatorio :: Int -> IO()
imprimirRelatorio op
  | op == 1 = putStr (relatorio 1 12)
  | op == 2 = 
    do
      putStr "Digite o semestre: "
      op <- getLine
      imprimirSemestre (read op)
  | op == 3 = 
    do
      putStr "Digite o trimestre: "
      op <- getLine
      imprimirTrimestre (read op)
  | op == 4 =
    do
      putStr "Digite o mes de inicial: "
      inicio <- getLine
      putStr "Digite o mes de final: "
      final <- getLine
      putStr (relatorio (read inicio) (read final))
  |otherwise =
    do
      putStr "Opcao invalida\n"
      main 

imprimirSemestre :: Int -> IO()
imprimirSemestre op
  | op == 1 = putStr (relatorio 1 6)
  | op == 2 = putStr (relatorio 7 12)
  | otherwise =
    do
      putStr "Opcao invalida\n"
      imprimirRelatorio 2
       
imprimirTrimestre :: Int -> IO()
imprimirTrimestre op
  | op == 1 = putStr (relatorio 1 3)
  | op == 2 = putStr (relatorio 4 6)
  | op == 3 = putStr (relatorio 7 9)
  | op == 4 = putStr (relatorio 10 12)
  | otherwise =
    do
      putStr "Opcao invalida\n"
      imprimirRelatorio 3

menuPrincipal :: String
menuPrincipal = "Opcoes disponiveis\n\t[1] Mostrar anual\n\t[2] Mostrar semestre\n\t[3] Mostrar trimestre\n\t[4] Mostrar intervalo personalizado\nOpcao: "

tamanho :: Int 
tamanho = 30

{-
A função relatorio recebe dois numeros inteiros
Os numeros determinam o primeiro e o último mes, respectivamente, a ser incluso no relatorio, podendo variar do mes 1 ao 12
-}
relatorio :: Int -> Int -> String
relatorio inicio fim = cabecalho ++ "\n" ++ corpo inicio fim ++ "\n" -- ++ rodape

cabecalho :: String
cabecalho = centralizarC " Empresa XPTO " 70 '-'

-- corpo = undefined
-- rodape = undefined

corpo :: Int -> Int -> String
corpo inicio fim = cabecalhoTabela ++ imprimeMeses inicio fim ++ rodape inicio fim ++ "\n\n" ++ grafico inicio fim

cabecalhoTabela :: String
cabecalhoTabela =  "MES">>>12 ++ "QUANTIDADE">>>11 ++"PRECO\n"

--imprimeMeses 1 = imprimeMes 1
imprimeMeses :: Int -> Int -> String
imprimeMeses inicio fim 
  | inicio == fim = imprimeMes inicio 
  | otherwise = imprimeMeses inicio (fim-1) ++ imprimeMes fim 

imprimeMes :: Int -> String
imprimeMes n = (mes n)>>>12 ++ ((show(vendas n))>>>11) ++ "R$ " ++ show(calculaPrecoVendaMes n) ++"\n" 

-- Abaixo se quiser centralizado
-- imprimeMes n = centralizar (impOcupandoEspacos (mes n) 12 ++ show(vendas n)) 70 ++ "\n" 

mes :: Int -> String
mes 1 = "Janeiro"
mes 2 = "Fevereiro"
mes 3 = "Marco"
mes 4 = "Abril"
mes 5 = "Maio"
mes 6 = "Junho"
mes 7 = "Julho"
mes 8 = "Agosto"
mes 9 = "Setembro"
mes 10 = "Outubro"
mes 11 = "Novembro"
mes 12 = "Dezembro"
{-
  media de vendas
  houve mes de vendas zeradas
  quantidade de meses com venda zerada
  maior venda
-}
espacamentoRodape :: Int
espacamentoRodape = 21

rodape :: Int -> Int -> String
rodape inicio fim =  imprimirSimbolo tamanho '-' ++ "\n" ++
          ("Soma Total: ">>>espacamentoRodape) ++ show (somavendasIntervalo inicio fim) ++ "\n" 
          ++ ("Valor Total: ">>>espacamentoRodape)++ "R$ " ++ (show(calculaPrecoVendaTotal inicio fim))>>>2 ++ "\n" ++
          ("Media de Vendas: ">>>espacamentoRodape) ++ show (mediaIntervalos inicio fim) ++ "\n" ++
          ("Desvio Padrao: ">>>espacamentoRodape)++ (show (desviopadrao inicio fim))>>>5 ++ "\n" ++
          ("Meses sem Vendas: ">>>espacamentoRodape) ++ show(quantidadeVendaZeradaIntervalo inicio fim) ++ "\n" ++
          ("Maior Venda: ">>>espacamentoRodape) ++ show(maiorVendaintervalo inicio fim) ++ "\n" ++
          ("Mes com Maior Venda: ">>>espacamentoRodape) ++ maiorMesDeVenda inicio fim fim ++ "\n"
          

vendas :: Int -> Int
vendas 1 = 10
vendas 2 = 12
vendas 3 = 8
vendas 4 = 0
vendas 5 = 30
vendas 6 = 23
vendas 7 = 23
vendas 8 = 9
vendas 9 = 19
vendas 10 = 13
vendas 11 = 22
vendas 12 = 18
vendas _  = 0

somavendas :: Int -> Int
somavendas 1 = vendas 1
somavendas n = vendas n + somavendas (n-1)

--------------------------------------------
-- Calculos de preco

preco :: Float
preco = 3450.30

calculaPrecoVendaMes :: Int -> Float
calculaPrecoVendaMes mes = fromIntegral (vendas mes) * preco

calculaPrecoVendaTotal :: Int -> Int -> Float
calculaPrecoVendaTotal inicio fim
  | inicio == fim = calculaPrecoVendaMes inicio
  | otherwise = calculaPrecoVendaMes fim + calculaPrecoVendaTotal inicio (fim-1)

--------------------------------------------------------
-- Implementar as funções abaixo utilizando recursão
-- Incluir as informações no relatório de Vendas
--------------------------------------------------------

-- Calcular a maior venda entre o mês 1 e n, inclusive
maiorVenda :: Int -> Int
maiorVenda n = maiorVendaAux n

maiorVendaAux :: Int -> Int
maiorVendaAux 1 = vendas 1
maiorVendaAux n = maior (vendas n) (maiorVendaAux (n-1))

maiorVendaintervalo :: Int -> Int -> Int
maiorVendaintervalo inicio fim = maiorVendaAuxIntervalo inicio fim

maiorVendaAuxIntervalo :: Int -> Int -> Int
maiorVendaAuxIntervalo inicio fim
  | inicio == fim = vendas inicio
  | otherwise = maior (vendas fim) (maiorVendaAuxIntervalo inicio (fim-1))

maior :: Int -> Int -> Int
maior a b = if a >= b then a else b

maiorMesDeVenda :: Int -> Int -> Int -> String
maiorMesDeVenda inicio fim mesMaior
  | inicio > fim = mes mesMaior
  | otherwise =
      if(vendas fim < vendas mesMaior) then
        maiorMesDeVenda inicio (fim-1) mesMaior
      else
        maiorMesDeVenda inicio (fim-1) fim

-- Verificar se há venda zerada entre o mês 1 e n, inclusive.
vendaZerada :: Int -> Bool
vendaZerada n
    | n == 1 = (vendas n) == 0
    | (vendas n) == 0  = True
    | otherwise =  vendaZerada (n-1)

-- Retorna a quantidade de vendas zeradas entre o mês 1 e n, inclusive
quantidadeVendaZerada :: Int -> Int
quantidadeVendaZerada n
    | n == 1    = boolParaInt ((vendas 1) == 0)
    | otherwise = boolParaInt ((vendas n) == 0)  + quantidadeVendaZerada(n-1)

quantidadeVendaZeradaIntervalo :: Int -> Int -> Int
quantidadeVendaZeradaIntervalo inicio fim
    | inicio == fim    = boolParaInt ((vendas inicio) == 0)
    | otherwise = boolParaInt ((vendas fim) == 0)  + quantidadeVendaZeradaIntervalo inicio (fim-1)


quadrado :: Float -> Float
quadrado n = n*n

desviopadrao :: Int -> Int -> Float
desviopadrao x y = sqrt( (resumoMedia x y y) /  (fromIntegral (y-x+1)))

resumoMedia::Int-> Int -> Int ->Float
resumoMedia x y n
  | x == y    = quadrado((fromIntegral (vendas x)) - mediaIntervalos x n) 
  | otherwise = quadrado((fromIntegral (vendas y)) - mediaIntervalos x n) + resumoMedia x (y-1) n 


-- ate aqui

mediaVendas :: Int -> Float
mediaVendas n = fromIntegral((somavendas n)) / (fromIntegral n)

--Denise
--Eric
mediaTrimestral :: Int -> Float
mediaTrimestral n 
  |n == 1 = mediaVendas 3
  |n == 2 = mediaVendas 6 - mediaVendas 3
  |n == 3 = mediaVendas 9 - mediaVendas 6
  |n == 4 = mediaVendas 12 - mediaVendas 9

mediaSemestral :: Int -> Float
mediaSemestral n 
  | n == 1  = mediaVendas 6
  | n == 2  = mediaVendas 12 - mediaVendas 6

mediaAnual :: IO()
mediaAnual = putStr (show (mediaVendas 12))


somavendasIntervalo :: Int -> Int -> Int
somavendasIntervalo x y = if x == y then vendas x else vendas y + somavendasIntervalo x (y-1) 

mediaIntervalos :: Int -> Int -> Float 
mediaIntervalos x y = fromIntegral((somavendasIntervalo x y)) / (fromIntegral (y-x+1))

{-
funcao grafico
i - Inicio do Intervalo
f - Fim do Intervalo
-}
grafico :: Int -> Int -> String
grafico i f = tituloGrafico ++ corpoGrafico i f ++ "\n" ++ fimGrafico

tituloGrafico :: String
tituloGrafico = centralizarC " GRAFICO VENDAS " 70 '=' ++ "\n"

{-
  funcao corpoGrafico
  i - Inicio do Intervalo
  m - Mes para recursao
-}
corpoGrafico :: Int -> Int -> String
corpoGrafico i m
  | m == i    = mesGrafico i
  | otherwise = (corpoGrafico i (m-1)) ++ mesGrafico m

mesGrafico :: Int -> String
mesGrafico n = (mes n)>>>10 ++ ": " ++ (imprimirSimbolo (vendas n) '#')>>>(maiorVenda 12) ++ " | " ++ show (vendas n) ++ "\n"

fimGrafico :: String
fimGrafico = imprimirSimbolo 20 '-' ++ "\n"

{-
------------------------------------------------
-- TO DO:

-- Melhorar layout do relatório conforme tamanho [done]
-- Centralizar os títulos [done]
-- Calcular desvio padrão [done]
-- Criar função Haskell para plotar gráfico de vendas [done]
-- Gerar relatório trimestral, semestral, anual, ... (função relatório deve receber parâmetro!!!)[done]
-- relatorio :: Int -> String [done]
-- relatorio :: Int -> Int -> String [done]
-- Incluir preço do produto. [done]
-- preco = 3450.30 [done]
-- Incluir no relatório o total de vendas por mês (R$) [done]
-- Incluir no relatório o total de vendas (R$) [done]
------------------------------------------------

-}


{-
    =========================================================
    ====================== STRING UTILS =====================
    =========================================================
-}

boolParaInt :: Bool -> Int
boolParaInt b = if b then 1 else 0

imprimirSimbolo :: Int -> Char -> String
imprimirSimbolo 0 ch = ""
imprimirSimbolo n ch = if n > 0 then [ch] ++ imprimirSimbolo (n-1) ch else ""

centralizar :: String -> Int -> String
centralizar str n = (imprimirSimbolo (div (n - length str) 2) ' ') ++ str ++ (imprimirSimbolo (div (n - length str) 2) ' ')

centralizarC :: String -> Int -> Char -> String
centralizarC str n c = (imprimirSimbolo (div (n - length str) 2) c) ++ str ++ (imprimirSimbolo (div (n - length str) 2) c)

impOcupandoEspacos :: String -> Int -> String
impOcupandoEspacos str n = if n < length str then str else str ++ (imprimirSimbolo (n - length str) ' ')

impOcupandoEspacosC :: String -> Int -> Char -> String
impOcupandoEspacosC str n c = if n < length str then str else str ++ (imprimirSimbolo (n - length str) c)

(>>>) :: String -> Int -> String
a >>> b = impOcupandoEspacos a b

(<|>) :: String -> Int -> String
a <|> b = centralizar a b