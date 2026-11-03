module Src.Classificacao where

import Src.Dados
import Src.Tipos
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

pontos :: Resultado -> Int
pontos resultado = 
    case resultado of
        Vitoria -> 3
        Empate -> 1
        Derrota -> 0
        
atualizarLinha :: Linha -> Partida -> Linha -- funcao auxiliar que analisa cada partida e retorna uma nova linha com campos atualizados (será usada na 'linhaDoTime' com 'foldl')
atualizarLinha linha partida
    | not (participou (time linha) partida) = linha
    | otherwise = 
        case resultadoPara (time linha) partida of
            Just Vitoria -> atualizar 1 0 0 gp gc
            Just Empate -> atualizar  0 1 0 gp gc
            Just Derrota -> atualizar 0 0 1 gp gc
            Nothing -> linha
            where 
                gp
                    | time linha == mandante partida = golsMandante partida
                    | otherwise = golsVisitante partida
                gc 
                    | time linha == mandante partida = golsVisitante partida
                    | otherwise = golsMandante partida
                atualizar v e d golsFeitos golsSofridos = Linha
                                                      (time linha)
                                                    (jogos linha + 1)
                                                    (vitorias linha + v)
                                                    (empates linha + e)
                                                    (derrotas linha + d)
                                                    (golsPro linha + golsFeitos)
                                                    (golsContra linha + golsSofridos)

linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime partidas time = foldl atualizarLinha linhaInicial partidas -- para cada partida do time, os campos da linha desse time sao atualizados combinando 'atualizarLinha' com 'foldl'
    where
        linhaInicial = Linha time 0 0 0 0 0 0 

pontosLinha :: Linha -> Int
pontosLinha linha = 3 * vitorias linha + empates linha

saldo :: Linha -> Int
saldo linha = golsPro linha - golsContra linha

aproveitamento :: Linha -> Double
aproveitamento linha 
    | jogos linha == 0 = 0
    | otherwise = (fromIntegral (pontosLinha linha) / 
                    fromIntegral (jogos linha * 3))  * 100

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada rodadaMax partidas = filter (\p -> rodada p <= rodadaMax) partidas

diferencaGols :: Partida -> Int
diferencaGols partida = abs (golsMandante partida - golsVisitante partida)

maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada [] = Nothing
maiorGoleada (primeira:resto) = 
    Just (foldl comparar primeira resto)
    where 
        comparar atual proxima 
            | diferencaGols proxima > diferencaGols atual = proxima
            | otherwise = atual

golsDaPartida :: Partida -> Int
golsDaPartida partida = golsMandante partida + golsVisitante partida

mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols partidas = Just (fromIntegral (sum (map golsDaPartida partidas)) / 
                            fromIntegral (length partidas))


capturarLinhas :: [Partida] -> [Linha]
capturarLinhas partidas = map (linhaDoTime partidas) (times partidas)

compararLinhas :: Linha -> Linha -> Ordering
compararLinhas l1 l2 = 
    comparing (Down . pontosLinha) l1 l2
    <> comparing (Down . vitorias) l1 l2
    <> comparing (Down . saldo) l1 l2
    <> comparing (Down . golsPro) l1 l2
    <> comparing time l1 l2

classificacao :: [Partida] -> [Linha]
classificacao partidas = sortBy compararLinhas (capturarLinhas partidas)
