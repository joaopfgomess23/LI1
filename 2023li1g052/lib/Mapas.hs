{- |
Module      : Tarefa1
Description : Niveis do jogo
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo que contem os niveis do jogo.
-}
module Mapas where

import LI12324
import MapRelated
import Tarefa4 (getAcao)
import LI12324 (Bloco(Plataforma, Vazio, Escada, Alcapao))

-- | A funcao __initialState__ recebe um mapa e transforma-o num jogo.
initialState :: Menu -> Mapa -> [Personagem] -> [(Colecionavel, Posicao)] -> Personagem -> Jogo
initialState menu mapa inimigos colecionaveis jogador = Jogo menu Mario mapa inimigos colecionaveis jogador

-- | a funcao __extrairJogoInicial__ retorna o meu mapa inicial
extrairJogoInicial :: (Jogo, Jogo) -> (Mapa, [Personagem], [(Colecionavel, Posicao)], Personagem)
extrairJogoInicial (Jogo _ skin1 mapa1 inimigos1 colecionaveis1 jogador1, Jogo _ skin2 mapa2 inimigos2 colecionaveis2 jogador2) =
    (mapa2, inimigos2, colecionaveis2, jogador2)

-- | Nivel 1 do jogo
nivel1 = (jogoInicial,jogoInicial)
  where 
    jogoInicial = initialState (Opcoes Jogar) mapa inimigos colecionaveis personagem
    posicao_inicial = (-19,-10)
    posicao_final = (-19,-4)
    personagem = Personagem (1,0) Jogador posicao_inicial Este (0,10) False False 3 0 (False, 0)
    inimigos = [ Personagem (1,0) Fantasma (8,-10) Oeste (1,1) False True 1 0 (False,0)
                ,Personagem (1,0) Fantasma (4,-4) Oeste (1,1) False True 1 0 (False,0)
                ,Personagem (1,0) MacacoMalvado (18.5,2) Oeste (1,1) False True 1 0 (False,0)
                ]
    colecionaveis = [(Martelo,(-17,-10)),
                      (Moeda, (10,-4)),
                      (Moeda, (-6,-8)),
                      (Moeda, (-8,-10))
                    ]
    mapa = Mapa (posicao_inicial, Este) (posicao_final)
              [[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, x, x, x, x, x, x, x, x, x, x, z, x, x, x, t, x, x, x, x, x, x, x, x, x, x, x, x, x, x, y, y, y, y, y, x, x, x]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[x, x, y, y, y, x, t, x, z, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, t, x, x, x, x, x, x]
              ,[y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[y, y, y, y, y, y, y, y, z, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y]
              ,[x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x]
              ]
              where
                x = Plataforma
                y = Vazio
                z = Escada
                t = Alcapao
