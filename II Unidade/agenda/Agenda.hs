module Agenda (
    criarContato,
    adicionarContato,

    mostrarContato,
    listarContatos,
    
    pesquisarContatoPorCpf,
    pesquisarContatoPorNome,
    
    apagarContatoPorCpf,
    apagarContatoPorNome,
    apagarContatoPorPosicao,
    
    alterarContatoPorCpf,
    alterarContatoPorNome,
    alterarContatoPorPosicao,
    
    carregarAgenda,
    salvarAgenda)
where

-- Imports necessarios
import Utils
import TiposAgenda
import Csv

{-
- acao:       criar um contato apos receber todas as informacoes necessarias.  
- entrada:    CPF, Nome, Telefone, Email.  
- saida:      ContatoData (dados do contato); 
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Antes do contato ser criado, primeiro tem que ser validado o cpf. Se o CPF for validade, entao o Contato eh criado.
- Se o CPF for invalido, entao o ContatoData eh dado como um contato invalido.
-}
criarContato :: CPF -> Nome -> Telefone -> Email -> ContatoData
criarContato cpf nome telefone email
    | validarCpf cpf =  Contato (filter ehDigito cpf) nome telefone email
    | otherwise = Invalido

{-
- acao:       adicionar um contato na agenda.  
- entrada:    ContatoData (contato que se deseja adicionar).  
- saida:      IO Agenda (contato adicionado na agenda); 
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se o status do Contato for dado como Invalido, entao ele soh retorna a agenda sem a adicao.
- Se o status do contato estiver normal, entao o contato vai ser adicionado agenda.
-}
adicionarContato :: ContatoData -> Agenda -> Agenda
adicionarContato Invalido agenda = agenda
adicionarContato contato agenda = agenda++[contato]
    {-do
        agendaDeArquivo <- carregarAgenda
        salvarAgenda (agendaDeArquivo++[contato])
        return (agendaDeArquivo++[contato])-}

{-
- acao:       mostrar os dados do contato passado.  
- entrada:    ContatoData (contato que se deseja mostrar), posicao (Int).  
- saida:      String (String para a amostragem do contato); 
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Vai fazer a formatacao dos dados do contato e passalo como String.
-}
mostrarContato :: ContatoData -> Int -> String
mostrarContato (Contato cpf nome telefone email) (-1) = "CPF: "++cpf ++ "\n" ++ "Nome: "++ nome ++ "\n" ++ "Telefone: " ++ telefone ++ "\n" ++ "E-mail: " ++ email
mostrarContato (Contato cpf nome telefone email) pos = "[" ++ (show pos) ++"] CPF: "++cpf ++ "\n" ++ "Nome: "++ nome ++ "\n" ++ "Telefone: " ++ telefone ++ "\n" ++ "E-mail: " ++ email

{-
- acao:       Vai mostrar todos os contatos cadastrados na agenda. Utiliza a funcao listarContatosAux.  
- entrada:    Agenda (Lista de contatos).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Vai mostrar os contatos da agenda utilizando o putStrLn e a funcao listaContatosAux.
-}
listarContatos :: Agenda -> IO()
listarContatos agenda = putStrLn (listarContatosAux agenda 0)
    {-do
        agenda <- carregarAgenda
        putStrLn (listarContatosAux agenda)-}

{-
- acao:       Vai percorrer a agenda e concatenar os contatos, retornando uma String.  
- entrada:    Agenda (Lista de contatos) e as posicoes (Int).  
- saida:      String (todos os contatos concatenados);
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a Agenda estiver vazia, retorna os contatos concatenados. 
- Se a Agenda nao estiver vazia, usa a funcao mostrarContato para receber a String do contato atual da agenda e concatena com os proximos contatos.
- O processo se repete ate nao houver mais contatos.
-}
listarContatosAux :: Agenda -> Int -> String
listarContatosAux [] _ = ""
listarContatosAux (contato:agenda) pos = (mostrarContato contato pos) ++ "\n\n" ++ (listarContatosAux agenda (pos+1) )

{-
- acao:       Vai pesquisar o contato usando alguma informacao. 
- entrada:    Dado(dado utilizado na pesquisa),Agenda (Lista de contatos), dadoDoContato (vai visualizar o dado do contato), posicao (Int).  
- saida:      posicaoDoContato (Int);
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se nao ouver mais contato, retorna a posicao -1, que significa que nao encontrou o contato.
- Se o valor for igual ao dado do contato atual que esta fazendo a procura, significa que foi encontrado.
- Se o dado nao for igual ao do contato, entao passa para a proxima posicao da lista. O processo se repete ate se encontrar ou nao o contato.
-}
pesquisarContato :: Dado -> Agenda -> (ContatoData -> Dado) -> Int -> Int
pesquisarContato _ [] _ _ = -1 
pesquisarContato valor (contato:agenda) retiraDado pos
    | valor == (retiraDado contato) = pos
    | otherwise = pesquisarContato valor agenda retiraDado (pos+1)

{-
- acao:       Vai pesquisar o contato usando o CPF. 
- entrada:    CPF,Agenda (Lista de contatos), posicao (Int).  
- saida:      posicaoDoContato (Int);
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  utiliza a funcao pesquisarContato para realizar a busca, passando o CPF e a posicao 0 como inicial da busca.
-}
pesquisarContatoPorCpf :: CPF -> Agenda -> Int
pesquisarContatoPorCpf cpf agenda = pesquisarContato cpf agenda cpfContato 0

{-
- acao:       Vai pesquisar o contato usando o Nome. 
- entrada:    Nome,Agenda (Lista de contatos), posicao (Int).  
- saida:      posicaoDoContato (Int);
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  utiliza a funcao pesquisarContato para realizar a busca, passando o Nome e a posicao 0 como inicial da busca.
-}
pesquisarContatoPorNome :: Nome -> Agenda -> Int
pesquisarContatoPorNome nome agenda = pesquisarContato nome agenda nomeContato 0

{-
- acao:       Vai apagar o Contato utilizando a posicao. 
- entrada:    posicaoDoContato (Int),Agenda (Lista de contatos).  
- saida:      Agenda (Agenda com o contato apagado)
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a agenda estiver vazia, entao retorna a propria agenda.
- Se a posicao for 0, ele retorna a agenda sem o contato da posicao.
- Se a posicao nao for 0, continua fazendo a busca diminuindo a posicao buscando pela agenda. Faz a concatenacao do contato (nao o da busca) nesse caso e faz a busca com a tail.
-}
apagarContatoPorPosicao :: Int -> Agenda -> Agenda
apagarContatoPorPosicao _  [] = []
apagarContatoPorPosicao 0 (contato:agenda) = agenda
apagarContatoPorPosicao posicao (contato:agenda) = contato:(apagarContatoPorPosicao (posicao-1) agenda)

{-
- acao:       Vai apagar o Contato utilizando o CPF. 
- entrada:    CPF,Agenda (Lista de contatos).  
- saida:      Agenda (Agenda com o contato apagado, ou nao se nao for encontrado).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a posicao for maior que -1, entao vai apagar o contato utilizando a funcao apagarContatoPorCpf, passando a posicao encontrada.
- Se nao, entao retorna a agenda sem alteracoes.
- A posicao eh encontrada utilizando a funcao pesquisarContatoPorCpf, que recebe o CPF e a agenda.
-}

apagarContatoPorCpf :: CPF -> Agenda -> Agenda
apagarContatoPorCpf cpf agenda
    | posicao > -1 = apagarContatoPorPosicao posicao agenda
    | otherwise = agenda
        where posicao = (pesquisarContatoPorCpf cpf agenda)

{-
- acao:       Vai apagar o Contato utilizando o Nome. 
- entrada:    Nome,Agenda (Lista de contatos).  
- saida:      Agenda (Agenda com o contato apagado, ou nao se nao for encontrado).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a posicao for maior que -1, entao vai apagar o contato utilizando a funcao apagarContatoPorPosicao, passando a posicao encontrada.
- Se nao, entao retorna a agenda sem alteracoes.
- A posicao eh encontrada utilizando a funcao pesquisarContatoPorNome, que recebe o Nome e a Agenda.
-}
apagarContatoPorNome :: Nome -> Agenda -> Agenda
apagarContatoPorNome nome agenda
    | posicao > -1 = apagarContatoPorPosicao posicao agenda
    | otherwise = agenda
        where posicao = (pesquisarContatoPorNome nome agenda)

{-
- acao:       Vai alterar o Contato utilizando a posicao. 
- entrada:    ContatoData (Contato),Agenda (Lista de contatos), posicao(Int).  
- saida:      Agenda (Agenda com o contato apagado, ou nao se nao for encontrado).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a agenda estiver vazia, entao retorna a propria agenda.
- Se o contato for Invalido, entao retorna a agenda sem alteracoes.
- Se a posicao for 0, entao o contato foi encontrado, retornando a agenda sem esse contato.
- Se a posicao nao for 0, entao continua a busca na lista, diminuindo 1 na posicao ateh chegar a 0.
-}
alterarContatoPorPosicao :: ContatoData -> Agenda -> Int -> Agenda
alterarContatoPorPosicao _ [] _ = []
alterarContatoPorPosicao Invalido agenda _ = agenda
alterarContatoPorPosicao contato (contatoAtual:agenda) 0 = contato:agenda
alterarContatoPorPosicao contato (contatoAtual:agenda) posicao = [contatoAtual]++(alterarContatoPorPosicao contato agenda (posicao-1))

{-
- acao:       Vai alterar o Contato utilizando o CPF. 
- entrada:    ContatoData (Contato),Agenda (Lista de contatos), CPF.  
- saida:      Agenda (Agenda com o contato apagado, ou nao se nao for encontrado).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a posicao for maior que -1, entao vai apagar o contato utilizando a funcao alterarContatoPorPosicao, passando a posicao encontrada.
- Se nao, entao retorna a agenda sem alteracoes.
- A posicao eh encontrada utilizando a funcao pesquisarContatoPorCpf, que recebe o CPF e a Agenda.
-}
alterarContatoPorCpf :: ContatoData -> Agenda -> CPF -> Agenda
alterarContatoPorCpf contato agenda cpf
    | posicao > -1 = alterarContatoPorPosicao contato agenda posicao
    | otherwise = agenda
    where
        posicao = pesquisarContatoPorCpf cpf agenda

{-
- acao:       Vai alterar o Contato utilizando o Nome. 
- entrada:    ContatoData (Contato),Agenda (Lista de contatos), Nome.  
- saida:      Agenda (Agenda com o contato apagado, ou nao se nao for encontrado).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a posicao for maior que -1, entao vai apagar o contato utilizando a funcao alterarContatoPorPosicao, passando a posicao encontrada.
- Se nao, entao retorna a agenda sem alteracoes.
- A posicao eh encontrada utilizando a funcao pesquisarContatoPorNome  que recebe o CPF e a Agenda.
-}
alterarContatoPorNome :: ContatoData -> Agenda -> Nome -> Agenda
alterarContatoPorNome contato agenda nome
    | posicao > -1 = alterarContatoPorPosicao contato agenda posicao
    | otherwise = agenda
    where
        posicao = (pesquisarContatoPorNome nome agenda)

{-
- acao:       vai converter um objeto Csv para agenda. 
- entrada:    CsvObj(Objeto a ser alterado),Agenda (Lista de contatos).  
- saida:      IO Agenda (transforma o Csv em agenda por meio do acumulador).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se o Objeto Csv estiver vazio, entao retorna o acumulador.
- Se o Objeto Csv for diferente de vazio, entao pegasse os dados do csvObj e enlista com o acumulador. 
- O processo de enlistar os dados com o acumulador continua ateh o Csv estiver vazio.
-}
converterParaAgenda :: CsvObj -> Agenda -> IO Agenda
converterParaAgenda [] acc = return acc
converterParaAgenda (registro:csvObj) acc
    | length registro < 4 = return []
    | otherwise           = converterParaAgenda csvObj ((Contato cpf nome telefone email):acc)
        where
            [cpf, nome, telefone, email] = registro

{-
- acao:       vai carregar a agenda utilizando caminho. 
- entrada:    acao de IO() (input/output).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O;
- suposicoes: nenhuma. 
- algoritmo:  Vai carregar a agenda utilizando a funcao do modulo Csv e a funcao converterParaAgenda, passando o objeto e a agenda vazia.
-}
carregarAgenda :: IO Agenda
carregarAgenda =
    do
        arquivoCarregado <- carregarArquivo "agenda.csv"
        converterParaAgenda arquivoCarregado []

{-
- acao:       vai converter uma  agenda para Csv. 
- entrada:    Agenda (Lista de contatos).  
- saida:      CsvObj(Objeto Csv).
- suposicoes: supoe-se que os parametros estejam corretos. 
- algoritmo:  Se a agenda estiver vazia, entao retorna uma lista vazia.
- Se a agenda nao estiver vazia, entao pegar os dados do contato atual da agenda e vai concatenar no Objeto Csv. O processo continua
ate nao haver mais informacoes a serem concatenadas, ou seja, nao houver mais contatos.
-}
agendaParaCsvObj :: Agenda -> CsvObj
agendaParaCsvObj [] = []
agendaParaCsvObj (registro:agenda) = [[cpf, nome, telefone, email]]++(agendaParaCsvObj agenda)
    where
        cpf = cpfContato registro
        nome = nomeContato registro
        telefone = telefoneContato registro
        email = emailContato registro

{-
- acao:       salvar a agenda utilizando a funcao escreverEmArquivo do Modulo Csv. 
- entrada:    Agenda (Lista de contatos).  
- saida:      tem tipo (), que é uma tupla vazia. Ou seja, a função não retorna nenhum resultado interessante, apenas faz I/O; 
- suposicoes: supoe-se que os contatos estejam corretos. 
- algoritmo:  Vai escrever os dados da agenda no arquivo passando o caminho e utilizando as funcoes escreverEmArquivo e agendaParaCsvObj.
-}
salvarAgenda :: Agenda -> IO()
salvarAgenda agenda = escreverEmArquivo "agenda.csv" (agendaParaCsvObj agenda)