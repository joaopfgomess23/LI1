{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe


import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Utils
import MapRelated
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{- |
A funcao __atualiza__ atualiza o estado do jogador, recebe como parametros uma lista de açoes aplicada a cada inimigo, a açao a aplicar ao jogador e o jogo que vai atualizar.
-}
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo@(Jogo menu _ mapa _ _ _) = 
    jogo { jogador = atualizaJogador mapa (jogador jogo) acaoJogador,
           inimigos = zipWith (\inimigo acao -> atualizaInimigo mapa inimigo acao) (inimigos jogo) acoesInimigos
         }
         

-- | a funcao __atualizaJogador__ atualiza a posicao do jogador fazendo a seguinte verificao: se o jogador nao estiver a colidir com nenhuma parede/bloco
--   realiza o movimento pretendido.
atualizaJogador :: Mapa -> Personagem -> Maybe Acao -> Personagem
atualizaJogador mapa jogador Nothing = jogador
atualizaJogador mapa@(Mapa (posi,diri) posf listablocos) jogador@(Personagem (xv,yv) tipo (x,y) dir tam escada ressalta vida ponto dano) (Just acao)
    | not (colisoesParede mapa (Personagem (xv,yv) tipo (posicao movimento) dir tam escada ressalta vida ponto dano)) = (Personagem (xv,yv) tipo (posicao movimento) dir tam escada ressalta vida ponto dano)
    | otherwise = Personagem (xv,yv) tipo (x,y) dir tam escada ressalta vida ponto dano

    where
        movimento = case acao of
            Subir -> if eEscada then jogador { velocidade = (0, 1), posicao = (x,y+1),direcao = Norte } else jogador {velocidade = (0,0), posicao = (x,y)}
            Descer -> if eEscada || escadaComoApoio then jogador { velocidade = (0, -1), posicao = (x,y-1), direcao = Sul} else jogador {velocidade = (0,0), posicao = (x,y)}
            AndarDireita -> if not eEscada || temblocoapioEscada then jogador { velocidade = (1,0), posicao = (x+1,y), direcao = Este} else jogador {velocidade = (0,0), posicao = (x,y), direcao = Este}
            AndarEsquerda -> if not eEscada || temblocoapioEscada then jogador { velocidade = (-1,0), posicao = (x-1,y), direcao = Oeste} else jogador {velocidade = (0,0), posicao = (x,y), direcao = Oeste} 
            Saltar -> if pode_saltar && temblocoapoioSeSaltar then jogador { velocidade = (0,2), posicao = (x,y+2), direcao = dir} else  jogador { velocidade = (0,0), posicao = (x,y), direcao = dir}

            Parar ->  jogador {velocidade = (0,0)}

        
        pode_saltar = acharBloco (round x,round y-1) (transformarMatriz listablocos constante) /= Escada
        temblocoapoioSeSaltar = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Plataforma ||
                        acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Escada
        temblocoapioEscada = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Plataforma
        eEscada = acharBloco (round x,round y-1) (transformarMatriz listablocos constante) == Escada
        escadaComoApoio = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Escada


-- | a funcao __atualizaInimigo__ atualiza a posicao do inimigo fazendo a seguinte verificao: se o inimigo nao estiver a colidir com nenhuma parede/bloco
--   realiza o movimento pretendido.
atualizaInimigo :: Mapa -> Personagem -> Maybe Acao -> Personagem
atualizaInimigo mapa inimigo Nothing = inimigo { velocidade = (0, 0) }
atualizaInimigo mapa@(Mapa (posi,diri) posf listablocos) inimigo@(Personagem (xv,yv) tipo (x,y) dir tam escada ressalta vida ponto dano) (Just acao)
    | not (colisoesParede mapa inimigo) && tipo == Fantasma = movimento
    | otherwise = if tipo == Fantasma then novoMovimento else inimigo

    where
        movimento = case acao of
            Subir -> if eEscada then mover (0, 1) Norte else inimigo { velocidade = (0, 0) }
            Descer -> if eEscada || escadaComoApoio then mover (0, -1) Sul else inimigo { velocidade = (0, 0) }
            AndarDireita -> if not eEscada || temblocoapioEscada then mover (1, 0) Este else inimigo { velocidade = (0, 0), posicao = (x+0.1,y)}
            AndarEsquerda -> if not eEscada || temblocoapioEscada then mover (-1, 0) Oeste else inimigo { velocidade = (0, 0), posicao = (x-0.1,y)}
            Parar -> inimigo { velocidade = (0, 0) }

        mover (dx, dy) newDir = inimigo { velocidade = (dx, dy), posicao = (x + dx/10, y + dy), direcao = newDir }

        novoMovimento = case direcao inimigo of
            Este -> inimigo { velocidade = (-1, 0), posicao = (x - 0.1, y), direcao = Oeste}
            Oeste -> inimigo { velocidade = (1, 0), posicao = (x + 0.1, y), direcao = Este}
            Norte -> inimigo { velocidade = (0, -1), posicao = (x, y - 0.1), direcao = Sul }
            Sul -> inimigo { velocidade = (0, 1), posicao = (x, y + 0.1), direcao = Norte }


        pode_saltar = acharBloco (round x,round y-1) (transformarMatriz listablocos constante) /= Escada
        temblocoapoioSeSaltar = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Plataforma ||
                        acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Escada
        temblocoapioEscada = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Plataforma
        eEscada = acharBloco (round x,round y-1) (transformarMatriz listablocos constante) == Escada
        escadaComoApoio = acharBloco (round x,round y-2) (transformarMatriz listablocos constante) == Escada

{- | A funcao __acaocontraria__ recebe como parametro uma acao e retorna a acao contraria.

== Exemplo

>>>acaocontraria AndarDireita
AndarEsquerda
-}
acaocontraria :: Acao -> Acao
acaocontraria acao | acao == AndarDireita = AndarEsquerda
                    | acao == AndarEsquerda = AndarDireita
                    | otherwise = Parar

-- | Funcao que inverte a direçao
inverterDirecao :: (Double, Double) -> (Double, Double)
inverterDirecao (x, y) = (-x, -y)

-- | a funcao __direcaoOposta__ recebe uma direcao e retorna a direcao oposta.
direcaoOposta :: Direcao -> Direcao
direcaoOposta dir =
    case dir of
        Norte -> Sul
        Sul -> Norte
        Este -> Oeste
        Oeste -> Este

-- | Funcao que converte a direcao de um vetor
direcaoToVector :: Direcao -> (Double, Double)
direcaoToVector dir =
    case dir of
        Norte -> (0, 1)
        Sul -> (0, -1)
        Este -> (1, 0)
        Oeste -> (-1, 0)


-- | A funcao __contarColisoes__ é uma funcao acumulativa que verifica se o inimigo colide com a parede e conta quantas vezes colide. Serve para dar movimentos aleatorios aos inimigos.
contarColisoes :: Int -> Mapa -> Personagem -> Int
contarColisoes 0 _ _ = 0
contarColisoes x mapa inimigo | colisoesParede mapa inimigo = 1+x
                              | otherwise = contarColisoes x mapa inimigo

-- | A funcao __getAcao__ recebe a tecla que e pressionada e associa-lhe uma acao.
getAcao :: Key -> Acao
getAcao (SpecialKey KeyUp) = Subir
getAcao (SpecialKey KeyDown) = Descer
getAcao (SpecialKey KeyLeft) = AndarEsquerda
getAcao (SpecialKey KeyRight) = AndarDireita
getAcao (SpecialKey KeySpace) = Saltar
