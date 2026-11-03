module Src.Tipos where

import Data.List (nub, sort)
import Data.Ord (comparing, Down)

type Time = String

data Resultado = Vitoria | Empate | Derrota
    deriving (Show, Eq)

data Partida =  Partida
    {   
        rodada :: Int, 
        mandante :: Time,
        golsMandante :: Int,
        golsVisitante :: Int,
        visitante :: Time  

    } deriving (Show, Eq)

data Linha = Linha
    {   
        time :: Time,
        jogos :: Int,
        vitorias :: Int,
        empates :: Int,
        derrotas :: Int,
        golsPro :: Int,
        golsContra :: Int

    } deriving (Show, Eq)

participou :: Time -> Partida -> Bool
participou time partida 
    | time == mandante partida = True
    | time == visitante partida = True
    | otherwise = False

resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t p
    | not (participou t p) = Nothing
    | golsDoTime > golsDoAdversario = Just Vitoria
    | golsDoTime < golsDoAdversario = Just Derrota
    | otherwise = Just Empate
    where
        golsDoTime
            | t == mandante p = golsMandante p
            | otherwise = golsVisitante p

        golsDoAdversario
            | t == mandante p = golsVisitante p
            | otherwise = golsMandante p

capturarTimes :: [Partida] -> [Time]
capturarTimes partidas = map mandante partidas ++ map visitante partidas -- pega todos os times das partidas, sejam mandantes ou visitantes 

times :: [Partida] -> [Time]
times partidas = sort (nub (capturarTimes partidas)) -- sort ordena e nub remove repeticoes

