module Src.Dados where

import Src.Tipos ( Partida(Partida) )

dadosCampeonato :: [Partida]
dadosCampeonato =
    [
        Partida 1 "Aurora" 0 0 "Ferroviario",
        Partida 1 "Bravos" 3 0 "Estrela",
        Partida 1 "Central" 1 2 "Dragons",

        Partida 2 "Aurora" 2 0 "Estrela",
        Partida 2 "Ferroviario" 0 4 "Dragons",
        Partida 2 "Bravos" 1 1 "Central",

        Partida 3 "Aurora" 0 3 "Dragons",
        Partida 3 "Estrela" 2 1 "Central",
        Partida 3 "Ferroviario" 1 0 "Bravos",

        Partida 4 "Aurora" 4 1 "Central",
        Partida 4 "Dragons" 0 2 "Bravos",
        Partida 4 "Estrela" 2 3 "Ferroviario",

        Partida 5 "Aurora" 1 0 "Bravos",
        Partida 5 "Central" 2 0 "Ferroviario",
        Partida 5 "Dragons" 1 1 "Estrela"
    ]
