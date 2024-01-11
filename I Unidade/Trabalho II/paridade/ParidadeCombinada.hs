{-
    Equipe:
        Alan Bomfim
        Breno Carvalho
        Denise Nogueira
        Eric
        Vitor Rosembergre
-}

module ParidadeCombinada(imprimirMatriz, corrigir, Matriz, Correcao) where

type Bit         = Int
type Bits        = [Bit]
type Matriz      = [Bits]
type Correcao    = (Matriz, String)

{-
- acao:       realiza a operacao booleana xor 
- entrada:    bit_1, bit_2; (bits para a operacao)
- saida:      bit_3; (bit resultante da operacao)
- suposicoes: supoe-se que os bits de entrada sejam 1 ou 0 
- algoritmo:  Operacao booleana xor entre o bit_1 xor bit_2 resultando no bit_3 como resultado.
-}
xor :: Bit -> Bit -> Bit
xor 1 0 = 1
xor 0 1 = 1
xor _ _ = 0

{-
- acao:       Percorrer a lista de bit's recebida e realizar a operacao xor entre os bits da lista. 
- entrada:    bits_1(lista de bit);  
- saida:      bit_1(bit resultante);
- suposicoes: A lista nao esteja vazia, de preferencia com 2 ou mais bits
- algoritmo:  Vai pegar os elementos da lista e fazer a operacao xor em conjunto. Exemplo: 1 xor 0 xor 1 = 0; 
-}
xorBits :: Bits -> Bit
xorBits [e] = e
xorBits (a:b) = xor a (xorBits b)

{-
- acao:       retornar a matriz teste 
- entrada:    nenhuma 
- saida:      matriz_1;
- suposicoes: nenhuma
- algoritmo:  ao chamar a funcao vai retornar a matriz abaixo.
-}
matriz :: Matriz
matriz = [
            [0,1,0,1,1],
            [1,1,0,1,0],
            [1,0,1,0,1],
            [1,1,0,1,1],
            [1,1,0,0,1]
         ]

{-
- acao:       imprimir cada linha da matriz que vai ser transformada em String com o auxilio da funcao ImprimirLinha. 
- entrada:    matriz_1(de Bits); 
- saida:      string_1(String representando a matriz);
- suposicoes: matriz nao esteja vazia
- algoritmo:  Imprimir a matriz, exemplo: Matriz[Bits] com 1 linha e 3 colunas, [0,1,0].
-}
imprimirMatriz :: Matriz -> String
imprimirMatriz [ultimaLinha] = (imprimirLinha ultimaLinha) ++ "\n"
imprimirMatriz (linha:matriz) =  (imprimirLinha linha) ++ "\n" ++ (imprimirMatriz matriz)

{-
- acao:       receber uma lista de Bit's e retorna sua String 
- entrada:    bits_1(lista de Bit's); 
- saida:      linha(String concatenada);
- suposicoes: lista nao esteja vazia
- algoritmo:  vai percorrer toda lista e concatenar os elementos com show(Bit), retornando a String concatenada. 
-}
imprimirLinha :: Bits -> String
imprimirLinha [ultimoBit] = show ultimoBit
imprimirLinha (bit:bits)  = (show bit) ++ " " ++ (imprimirLinha bits)

{-
- acao:       usa composicao de funcao para gerar uma matriz com bits de paridade
- entrada:    matriz_1(matrix sem bits de paridade); 
- saida:      matriz_2(matrix com bits de paridade);
- suposicoes: matriz nao esteja vazia
- algoritmo:  gerar uma matriz com bits de paridade a partir de uma matriz de bits, gerando assim a paridade combinada
-}
gerarParidadeMatriz :: Matriz -> Matriz
gerarParidadeMatriz matriz = gerarParidadeColuna (gerarParidadeLinha matriz)

{-
- acao:       recebe uma matiz e usa recursao para acessar cada elemento dentro dela, que nesssa abstracao seria a linha.
- entrada:    matriz_1(matriz de entrada); 
- saida:      matriz_2(matriz de saida);
- suposicoes: a matriz nao esteja vazia
- algoritmo:  A funcao acessa cada linha da matriz e usa a funcao xorBits
  para gerar o bit de paridade que eh adicionado no final dessa linha, usando recursao para gerar
  a matriz que possui paridades em suas linhas
-}
gerarParidadeLinha :: Matriz -> Matriz
gerarParidadeLinha [e] = [e ++ [xorBits e]]
gerarParidadeLinha (a:b) = [a ++ [xorBits a]] ++ gerarParidadeLinha b 

{-
- acao:       Gera a ultima linha de bits, que são os bits de paridade relacionados a coluna. 
- entrada:    matriz_1(matriz de entrada); 
- saida:      matriz_2(matriz de saida);
- suposicoes: matriz nao esteja vazia
- algoritmo:  para gerar a ultima linha de bits, usa uma funcao auxiliar, que retorna uma lista de bits, que sera 
  adicionada no final da matriz,
-}
gerarParidadeColuna :: Matriz -> Matriz
gerarParidadeColuna matriz = matriz ++ [gerarParidadeColunaAuxiliar matriz (length (matriz !! 0)-1)]

{-
- acao:       gerar uma lista com os bits. 
- entrada:    matriz_1(matriz de entrada), index(indice da lista, percorre a lista, o valor passado vai ser o tamanho da matriz - 1);; 
- saida:      bits_1(bits de paridade);
- suposicoes: a matriz nao esteja vazia e index esteja correto.
- algoritmo:   usa a 
  compreensão de lista para gerar uma lista com os bits de uma coluna, depois chama a
  funcao xorBits para gerar o bit de paridade e usa recursao para gerar uma lista.
-}
gerarParidadeColunaAuxiliar :: Matriz -> Int -> Bits
gerarParidadeColunaAuxiliar matriz 0 = [xorBits [(linha !! 0) | linha <- matriz]]
gerarParidadeColunaAuxiliar matriz index =  gerarParidadeColunaAuxiliar matriz (index -1) ++ [xorBits coluna]
  where
    coluna = [(linha !! index) | linha <- matriz]

{-
- acao:       verificar se as linhas e colunas estao corretas, o processo eh feito separadamente. 
- entrada:    matriz_1(matriz de entrada); 
- saida:      bool(True se a informacao passada estiver correta, e False se o contrario)
- suposicoes: matriz nao esteja vazia
- algoritmo:  usa de duas funcoes para verificar a corretude da informacao, diferentes funcoes para a verificar as linhas e colunas.
-}
verificarMatriz :: Matriz -> Bool
verificarMatriz matrixDeBits = (verificarLinhas matrixDeBits) && (verificarColunas matrixDeBits)

{-
- acao:       verifica se a linha esta correta 
- entrada:    bits_1(linha),inteiro_1(acumulador); 
- saida:      bool(verificar se a linha eh valida);
- suposicoes: lista nao esteja vazia  
- algoritmo:  consideramos o fato de que
   o xor inverte os bits quando um bit atual eh 1, entao caso seja par, a saida deve ser 0.
   Trabalha como um reducer.
-}
verificarLinha :: Bits -> Int -> Bool
verificarLinha [bitVerificador]  acc = (bitVerificador `xor` acc) == 0
verificarLinha (bit:arrayDeBits) acc = verificarLinha (arrayDeBits) (acc `xor` bit)

{-
- acao:       faz a verificacao das linhas da matriz 
- entrada:    matriz_1(matriz de entrada); 
- saida:      bool(verifica se a matriz eh valida);
- suposicoes: a matriz nao esteja vazia
- algoritmo:  utiliza da funcao verificar linha para verificar todas as linhas de forma recursiva, percorrendo uma linha por vez.
-}
verificarLinhas :: Matriz -> Bool
verificarLinhas   [linhaDeBits] = verificarLinha linhaDeBits 0
verificarLinhas (linhaDeBits : matrixDeBits) = (verificarLinha linhaDeBits 0) && (verificarLinhas matrixDeBits)

{-
- acao:       verifica e retorna quais linhas da matriz possuem erros. 
- entrada:    matriz_1(matriz de entrada);
- saida:      lista_1(lista de inteiros com as linhas que possuem erros);
- suposicoes: a matriz nao esteja vazia.
- algoritmo:  utiliza da funcao linhasComErroAuxiliar para verificar as linhas com erros. 
-}
linhasComErro :: Matriz -> [Int]
linhasComErro matriz = linhasComErroAuxiliar matriz 0

{-
- acao:       verificar as linhas que possuem erros 
- entrada:    matriz_1(matriz de entrada), contador(contador, vai representar a linha da matriz); 
- saida:      lista_1(lista com as linhas erradas);
- suposicoes: matriz nao esteja vazia e o contador inicialize com 0 (ou seja passado com o valor 0);
- algoritmo:  verifica cada linha da matriz por meio da funcao verificarLinha, se for valida, continua a busca na matriz, se nao, adiciona na lista qual linhas que nao eh valida. 
-}

linhasComErroAuxiliar :: Matriz -> Int -> [Int]
linhasComErroAuxiliar [] cont = []
linhasComErroAuxiliar (linhaDeBits : matrixDeBits) cont = 
  if (verificarLinha linhaDeBits 0) == False then
    [cont] ++ (linhasComErroAuxiliar matrixDeBits (cont+1))
  else
    [] ++ (linhasComErroAuxiliar matrixDeBits (cont+1))

{-
- acao:       Verifica se as colunas recebidas chegaram corretamente. 
- entrada:    matriz_1(matriz de entrada); 
- saida:      bool(representa se a matriz chegou corretamente);
- suposicoes: matriz nao esteja vazia
- algoritmo:  foi utilizado a funcao auxiliar verificarColunasAux, que recebe a matriz para fazer a verificacao das colunas e o numero da coluna.
-}
verificarColunas :: Matriz -> Bool
verificarColunas matrizDeBits = verificarColunasAux matrizDeBits (length (matrizDeBits!!0)-1)


{-
- acao:       faz o xor de todas as colunas da matriz. 
- entrada:    matriz_1(matriz de entrada), coluna(coluna que vai ser realizada a operacao);
- saida:      bool(se a coluna eh valida);
- suposicoes: a matriz nao esteja vazia
- algoritmo:  utiliza das funcoes verificarLinha e verificarColunasAux para verificar se as colunas sao validas, vai passar por todas as colunas usando o numeroCol = 0 como parada.
-}
verificarColunasAux :: Matriz -> Int -> Bool
verificarColunasAux matriz 0 = (verificarLinha [(linha !! 0) | linha <- matriz] 0)
verificarColunasAux matriz numeroCol = (verificarLinha coluna 0) && (verificarColunasAux matriz (numeroCol-1))
  where
    coluna = [(linha !! numeroCol) | linha <- matriz]

{-
- acao:       verificar quais colunas com erro; 
- entrada:    matriz_1(matriz de entrada); 
- saida:      lista_1(lista de colunas erradas);
- suposicoes: matriz nao esteja vazia;
- algoritmo:  utiliza da funcao colunasComErroAuxiliar para verificar as linhas com erros.
-}
colunasComErro :: Matriz -> [Int]
colunasComErro matrizDeBits = colunasComErroAuxiliar matrizDeBits (length (matrizDeBits!!0)-1)


{-
- acao:       verificar as colunas que possuem erros  
- entrada:    matriz_1(matriz de entrada), qnt_colunas(numero de colunas); 
- saida:      lista_1(lista de inteiros representando as linhas com erros);
- suposicoes: a matriz nao esteja vazia e o numero de colunas esteja correto;
- algoritmo:  verifica cada coluna da matriz por meio da funcao verificarLinha;
-}
colunasComErroAuxiliar :: Matriz -> Int -> [Int]
colunasComErroAuxiliar matriz (-1) = []
colunasComErroAuxiliar matriz numeroCol = 
  if (verificarLinha coluna 0) == False then
    [numeroCol] ++ (colunasComErroAuxiliar matriz (numeroCol-1))
  else
    [] ++ (colunasComErroAuxiliar matriz (numeroCol-1))
  where
    coluna = [(linha !! numeroCol) | linha <- matriz]


{-
- acao:       analise e se possivel correcao de dados da matriz;
- entrada:    matriz_1(matriz de entrada); 
- saida:      correcao_1(matriz, e string com um relatorio);
- suposicoes: matriz nao esteja vazia;
- algoritmo:  verificar se a matriz foi transmitida corretamente, se nao houver erro ela retorna a matriz passada,se tiver apenas 1 erro ela corrige, se tiver mais de um erro a funcao avisa que tem erro.
-}
corrigir :: Matriz -> Correcao
corrigir matriz
  | (quantidadeLinhas == quantidadeColunas) && quantidadeLinhas == 1 = (novaMatriz,"Ha um erro que foi corrigido")
  | (quantidadeLinhas == quantidadeColunas) && quantidadeLinhas == 0 = (matriz,"Nao ha erro")
  | otherwise = (matriz,"Ha erro, mas nao pode ser corrigido")
  where
    linhasErradas     = linhasComErro matriz
    colunasErradas    = colunasComErro matriz
    quantidadeLinhas  = length linhasErradas
    quantidadeColunas = length colunasErradas
    novaMatriz        = gerarMatrizCorrigida matriz (linhasErradas !! 0) (colunasErradas !! 0) (length matriz - 1)

{-
- acao:       gera matriz corrigida (eh usada na funcao corrigir) 
- entrada:    matriz_1(matriz para correcao), linha(que tem o erro), coluna(coluna que tem o erro), cont (saber se chegou na linha que contem o erro);
- saida:      matriz_2(matriz corrigida);
- suposicoes: matriz nao pode estar vazia, linha, coluna e contador passados corretamente.
- algoritmo:  usa a matriz a anterior para montar a nova matriz(matriz corrigida), (funcoes usadas: substituiBit);
-}
gerarMatrizCorrigida :: Matriz -> Int -> Int -> Int -> Matriz
gerarMatrizCorrigida matriz linha coluna cont
  | cont == 0 = [(matriz !! cont)]
  | cont == linha = (gerarMatrizCorrigida matriz linha coluna (cont-1)) ++ [substituiBit (matriz !! linha) coluna 0]
  | otherwise = (gerarMatrizCorrigida matriz linha coluna (cont-1)) ++ [(matriz !! cont)]

{-
- acao:       inverter o bit que estah errado (na matriz);
- entrada:    linha_1(linha da matriz que contem o erro), coluna_1(posicao na linha), contador; 
- saida:      linha_2(linha da matriz corrigida, o bit com o erro foi corrigido);
- suposicoes: a linha nao pode estar vazia, coluna e contador nao podem estar errados
- algoritmo:  vai alcancar o bit corrompido e corrigi-lo 
-}
substituiBit :: Bits -> Int -> Int -> Bits
substituiBit [] coluna cont = [] 
substituiBit (bit:bits) coluna cont = 
  if (cont == coluna) then
    [inverterBit bit] ++ substituiBit bits coluna (cont+1)
  else
    [bit] ++ substituiBit bits coluna (cont+1)

{-
- acao:       inverter o bit de entrada 
- entrada:    bit_1(bit de entrada); 
- saida:      bit_2(bit de entrada invertido);
- suposicoes: bit sendo 0 ou 1
- algoritmo:  todas a opcoes possiveis na funcao, se for diferente de 0/1 o retorno eh -1;
-}
inverterBit :: Bit -> Bit
inverterBit 0 = 1
inverterBit 1 = 0
inverterBit _ = (-1)
