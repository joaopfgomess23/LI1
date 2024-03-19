module Main where

import Test.HUnit
import LI12324
import Utils
import MapRelated
import Tarefa1
import Tarefa2
import Tarefa3

--------------------------------------------------------------------------- tarefa1
mapa1 = Mapa ((0, 0), Oeste) (0, 0) [ [Vazio, Vazio, Vazio, Vazio, Vazio]
                                    , [Plataforma, Plataforma, Plataforma, Vazio, Vazio]
                                    , [Vazio, Vazio, Vazio, Vazio, Vazio]
                                    , [Vazio, Vazio, Plataforma, Vazio, Vazio]
                                    , [Vazio, Vazio, Vazio, Vazio, Vazio]
                                    ]
mapa2 = (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
personagem1 = Personagem (1,0) Jogador (-20, 0) Norte (0,10) False False 3 0 (False, 0) -- esta fora dos limites do jogo
personagem2 = Personagem (1,0) Jogador (-19, 4) Norte (0,10) False False 3 0 (False, 0) -- esta na posicao de um bloco p
personagem3 = Personagem (1,0) Jogador (-19, 4) Norte (0,10) False False 3 0 (False, 0) -- esta dentro dos limites


p1 = Personagem (1,1) Fantasma (-18,4) Este (10,20) False True 1 0 (False,0)
p2 = Personagem (1,1) Fantasma (1,0) Este (10,20) False True 1 0 (False,0)
---------------------------------------------------------------------------------------------------------------------------------
tarefa1 = test ["tarefa1: personagem a colidir com parede" ~: True ~=? colisoesParede mapa1 personagem1,
                "tarefa1: personagem a colidir com bloco plataforma" ~: True ~=? colisoesParede mapa2 personagem2,
                "tarefa1: personagem que nao coluda com a parede com parede" ~: False ~=? colisoesParede mapa1 personagem3,
                "tarefa1: personagens com as mesmas coordendas (colidem)" ~: True ~=? colisoesPersonagens personagem2 personagem3,
                "tarefa1: personagens com 1 x de diferença (colidem devido a hitbox)" ~: True ~=? colisoesPersonagens personagem3 p1,
                "tarefa1: personagens com coordenadas diferentes" ~: False ~=? colisoesPersonagens p1 p2
                ]

--------------------------------------------------------------------------- tarefa2
jogo1 = Jogo (Opcoes Jogar) Mario -- respeita todas as regras
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    , Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

jogo2 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra1
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]) -- existe um boraco no chao
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    , Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

jogo3 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra3
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    , Personagem (1,1) Fantasma (2,2) Norte (1,2) False True 1 0 (False,0)
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0)) -- colide com um inimigo na posicao inicial

jogo4 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra4
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0) -- so tem 1 inimigo
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

jogo5 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra5
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 2 0 (False,0), -- fantasma tem 2 vidas
      Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

jogo6 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra6
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0),
      Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    ]
    []
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

jogo8 = Jogo (Opcoes Jogar) Mario -- desrespeita a regra8
    (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) 
    [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    , [Plataforma, Plataforma, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Escada, Plataforma]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ])
    [ Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 2 0 (False,0),
      Personagem (1,1) Fantasma (1,1) Norte (1,2) False True 1 0 (False,0)
    ]
    [(Martelo,(-19,4))] -- esta na posicao de uma plataformma
    (Personagem (1,2) Jogador (2,2) Sul (1,2) False False 3 0 (False,0))

---------------------------------------------------------------------------------------------------------------------------------
tarefa2 = test ["tarefa2: o mapa respeita todos os 6 requisitos" ~: True ~=? valida jogo1,
                "tarefa2: o mapa desrespeita o 1º requisito" ~: False ~=? valida jogo2,
                "tarefa2: o mapa desrespeita o 2º requisito" ~: False ~=? valida jogo3,
                "tarefa2: o mapa desrespeita o 4º requisito" ~: False ~=? valida jogo4,
                "tarefa2: o mapa desrespeita o 5º requisito" ~: False ~=? valida jogo5,
                "tarefa2: o mapa desrespeita o 6º requisito" ~: False ~=? valida jogo6,
                "tarefa2: o mapa desrespeita o 8º requisito" ~: False ~=? valida jogo8
                ]

--------------------------------------------------------------------------- tarefa3


---------------------------------------------------------------------------------------------------------------------------------
tarefa3 = test ["tarefa3: o mapa respeita todos os 6 requisitos" ~: True ~=? valida jogo1
                ]
main :: IO ()
main = runTestTTAndExit $ test [tarefa1,tarefa2,tarefa3]

