module Src.Leituras where

import Src.Tipos

exibirLinha :: Linha -> IO ()
exibirLinha linha =
    putStrLn (
        time linha ++ " | " ++
        show (jogos linha) ++ " | " ++
        show (vitorias linha) ++ " | " ++
        show (empates linha) ++ " | " ++
        show (derrotas linha) ++ " | " ++
        show (golsPro linha) ++ " | " ++
        show (golsContra linha)
    )

exibirTabela :: [Linha] -> IO ()
exibirTabela linhas = do
    putStrLn "\n========== TABELA DE CLASSIFICACAO =========="
    mapM_ (\(posicao, linha) -> do
        putStr (show posicao ++ "º | ")
        exibirLinha linha
        ) (zip [1..] linhas)
        

exibirPartida :: Partida -> IO ()
exibirPartida partida =
    putStrLn (
        "Rodada " ++ show (rodada partida) ++
        " | " ++ mandante partida ++
        " " ++ show (golsMandante partida) ++
        " x " ++ show (golsVisitante partida) ++
        " " ++ visitante partida
    )
    
exibirPartidas :: [Partida] -> IO ()
exibirPartidas partidas = do
    putStrLn "\n========== PARTIDAS DO CAMPEONATO =========="
    mapM_ exibirPartida partidas

    