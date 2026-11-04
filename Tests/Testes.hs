module Main where

import Tipos
import Dados
import Classificacao
import Leituras
import Data.List (nub, sort)

verificar :: String -> Bool -> IO() -- funcao auxiliar para mostrar se um determinado teste passou
verificar nomeTeste resultado =
    if resultado
        then putStrLn("[PASSOU] " ++ nomeTeste)
        else putStrLn("[FALHOU] " ++ nomeTeste)


testeParticipou :: IO()
testeParticipou = do
    let partida = head dadosCampeonato -- head pega a primeira partida do campeonato de teste

    verificar "Mandante participa" (participou "Aurora" partida)
    verificar "Visitante participa" (participou "Ferroviario" partida)
    verificar "Time que nao participa" (not (participou "Bravos" partida))


testeResultadoPara :: IO()
testeResultadoPara = do
    let partida = head dadosCampeonato

    verificar "Aurora empatou" (resultadoPara "Aurora" partida == Just Empate)
    verificar "Ferroviario empatou" (resultadoPara "Ferroviario" partida == Just Empate)
    verificar "Bravos nao participou" (resultadoPara "Bravos" partida == Nothing)

    let partida2 = dadosCampeonato !! 1 -- indice 1 (segunda partida)

    verificar "Bravos venceu" (resultadoPara "Bravos" partida2 == Just Vitoria)
    verificar "Estrela perdeu" (resultadoPara "Estrela" partida2 == Just Derrota)

testeTimes :: IO()
testeTimes = do
    let resultado = times dadosCampeonato

    verificar "Existem 6 times diferentes" (length resultado == 6)
    verificar "Nao existem times repetidos" (length resultado == length(nub resultado))
    verificar "Times em ordem alfabetica" (resultado == sort resultado)

    verificar "Lista de times correta" (resultado == 
                                            [
                                                "Aurora",
                                                "Bravos",
                                                "Central",
                                                "Dragons",
                                                "Estrela",
                                                "Ferroviario"
                                            ])

testeEstruturaCampeonato :: IO()
testeEstruturaCampeonato = do
    verificar "Pelo menos 6 times" (length (times dadosCampeonato) >= 6)
    verificar "Pelo menos 5 rodadas" (length (nub (map rodada dadosCampeonato)) >= 5)

testeEmpateSemGols :: IO()
testeEmpateSemGols =      -- verificando se existe alguma partida (any) que satisfaça a condicao (empate 0 a 0)
    verificar "Existe empate 0 a 0" (any (\p -> golsMandante p == 0 &&  
                                                    golsVisitante p == 0) dadosCampeonato)
            

testeGoleada :: IO()
testeGoleada =          -- verificando alguma partida com uma vitoria de 3 ou mais gols de diferenca ('abs' para tratar valor negativo)
    verificar "Existe goleada" (any (\p -> abs (golsMandante p - golsVisitante p) >= 3) dadosCampeonato)

testeConflitosRodada :: IO()
testeConflitosRodada = 
    verificar "Nenhum time joga 2 vezes na mesma rodada"
    (all rodadaValida (nub (map rodada dadosCampeonato))) -- pega uma lista de diferentes rodadas [ex. 1,2,3,4,5]
        where
            rodadaValida numero = 
                let partidasRodada =    -- para cada rodada agrupa as partidas correspondentes (ex. 3 partidas na rodada 1)
                        filter (\p -> rodada p == numero) dadosCampeonato

                    participantes =     -- concatMap agrupa todos os times (mandante e visitante) de cada partida
                        concatMap (\p -> [mandante p, visitante p]) partidasRodada
                in length participantes == length (nub participantes)  -- verifica se a quantidade de times encontrada é igual à quantidade sem repeticoes 
                
main :: IO()
main = do

    putStrLn("\n===== TESTES DA CAMADA 1 =====\n")
    testeParticipou
    testeResultadoPara
    testeTimes
    
    putStrLn("\n===== TESTES DO CAMPEONATO =====\n")
    testeEstruturaCampeonato
    testeEmpateSemGols
    testeGoleada
    testeConflitosRodada

    putStrLn("\n===== TESTE LISTAGEM RODADAS (ATE RODADA 3) =====")
    exibirPartidas (ateRodada 3 dadosCampeonato)

    putStrLn("\n===== TESTE LINHA DO TIME NA CLASSIFICACAO =====")
    exibirLinha (linhaDoTime dadosCampeonato "Aurora")

    --putStrLn("\n===== TESTE TABELA CLASSIFICACAO =====\n")
    --print(classificacao dadosCampeonato)
    
    exibirTabela(classificacao dadosCampeonato)
