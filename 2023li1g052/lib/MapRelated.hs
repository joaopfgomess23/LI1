{- |
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo que contem as funcoes sobre o estado do mapa.
-}
module MapRelated where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Data.List

-- | Dicionario que associa uma string a uma imagem
type Imagens = [(String,Picture)]

-- | Associa um jogo as suas respetivas imagens
type EstadoMapa = (Jogo, Imagens)

-- | a funcao __transformarMatriz__ recebe uma lista de blocos e associa-lhes a sua posicao real da matriz.
transformarMatriz :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
transformarMatriz [] x = []
transformarMatriz mapa@((h:t)) x = transformaLinha (h) (-19) x : transformarMatriz t (x+1)

-- | Funcao auxiliar de __transformarMatriz__.
transformaLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
transformaLinha [] _ _ = []
transformaLinha (bloco:rblocos) x y = (bloco, (x,y)) : transformaLinha rblocos (x+1) y

-- | A Funcao __desenhaMapa__ recebe dois argumentos, um tuplo com o jogo e as imagens e uma lista de listas de tuplos de blocos e posicoes e associa
--  graficamente, de acordo com as posicoes passadas, uma imagem ao bloco.
desenhaMapa :: EstadoMapa -> [[(Bloco,(Int,Int))]] -> Picture
desenhaMapa _ [] = blank
desenhaMapa estadogloss@((Jogo _ _ mapa _ _ _), images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa estadogloss t]

-- | Funcao auxiliar de __desenhaMapa__ que associa as imagens aos blocos.
desenhaLinhaMapa :: Imagens -> [(Bloco, (Int,Int))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of
            Plataforma -> Pictures [Translate ((fromIntegral x)*15.35) (-(fromIntegral y)*15.35) $ Scale 0.0965 0.16999 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
            Escada -> Pictures [Translate ((fromIntegral x)*15.35) (-(fromIntegral y)*15.35) $ Scale 0.16999 0.16999 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
            Vazio -> Pictures [Translate ((fromIntegral x)*15.35) (-(fromIntegral y)*15.35) $ Scale 0.16999 0.16999 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
            Alcapao -> Pictures [Translate ((fromIntegral x)*15.35) (-(fromIntegral y)*15.35) $ Scale 0.1 0.16999 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]

-- | A funcao __dentroDosLimites__ garante que o personagem se encontra dentro dos limites da matriz. (as matrizes tem um comprimento/largura pre definidos)
dentroDosLimites :: (Double, Double) -> Bool
dentroDosLimites (x, y) = x >=(-19) && x <= 19 && y >= (-12) && y <= 12

-- | A funcao __acharBloco__ recebe uma posicao e uma matriz transformada (com tuplos de blocos e a respetiva coordenada) e retorna o bloco em que se encontra.
acharBloco :: (Int,Int) -> [[(Bloco,(Int,Int))]] -> Bloco
acharBloco _ [] = Vazio
acharBloco (x,y) ([]:t) = acharBloco (x,y) t
acharBloco (x,y) ((h:t):t1) = if acharBlocoLinha (x,y) (h:t) == Vazio then acharBloco (x,y) t1 else acharBlocoLinha (x,y) (h:t) 

-- | Funcao auxiliar de __acharBloco__ que retorna o bloco em que se encontra numa posicao, indicida como primeiro argumento.
acharBlocoLinha :: (Int, Int) -> [(Bloco, (Int, Int))] -> Bloco
acharBlocoLinha _ [] = Vazio
acharBlocoLinha (x,y) ((bloco,(xbloco,ybloco)):t) | x == xbloco && y == - ybloco = bloco
                                                  | otherwise = acharBlocoLinha (x,y) t
