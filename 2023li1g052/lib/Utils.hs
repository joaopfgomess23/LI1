{- |
Module      : Tarefa1
Description : Funcoes uteis
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo que contem funcoes uteis que servem de auxilio a tarefas e ao codigo em geral.
-}
module Utils where

import LI12324
import Graphics.Gloss
import MapRelated
import Graphics.Gloss.Interface.Pure.Game


---------------------------
----------- tarefa 1

-- | funcao de debug, desenha a hitbox no meu personagem
drawHitbox :: Hitbox -> Picture
drawHitbox ((x1, y1), (x2, y2)) = lineLoop [(realToFrac x1, realToFrac y1), (realToFrac x1,realToFrac y2), (realToFrac x2,realToFrac y2), (realToFrac x2,realToFrac y1)]

-- | funcao de debug, desenha a hitbox X pixeis a frente do meu personagem, de acordo com a direcao.
drawHitboxFrente :: Hitbox -> Direcao -> Picture
drawHitboxFrente ((x1, y1), (x2, y2)) dir =
  let
    novaHitbox = case dir of
      Oeste -> ((x1 - 20, y1), (x2 - 20, y2))
      Este  -> ((x1 + 20, y1), (x2 + 20, y2))
      Norte -> ((x1, y1 + 20), (x2, y2 + 20))
      Sul   -> ((x1, y1 - 20), (x2, y2 - 20))
  in
    Pictures [drawHitbox novaHitbox]

-- | A funcao __colideHitBoxes__ verifica se dadas duas hitboxes e duas direçoes os personagens colidem, isto é, se se encontram praticamente na mesma
--   coordenada x e y.
colideHitBoxes :: Hitbox -> Direcao -> Hitbox -> Direcao -> Bool
colideHitBoxes ((x1, y1), (x2, y2)) dir1 ((x3, y3), (x4, y4)) dir2 =
    case (dir1, dir2) of
        (Norte, Sul) -> y1 < y4 && y2 > y3 && x1 < x4 && x2 > x3
        (Sul, Norte) -> y3 < y2 && y4 > y1 && x3 < x2 && x4 > x1
        (Este, Oeste) -> x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3
        (Oeste, Este) -> x3 < x2 && x4 > x1 && y3 < y2 && y4 > y1
        _ -> x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3

-- | funcao auxiliar de __colideHitBoxes__. Garante que se os jogadores tiverem direçoes simetricas existe uma chance de colidir.
vaoColidir :: Direcao -> Direcao -> Bool
vaoColidir Oeste Este = True
vaoColidir Este Oeste = True
vaoColidir Norte Sul = True
vaoColidir Sul Norte = True
vaoColidir _ _ = False

-- | A funcao __getPersonagemHitbox__ recebe como argumento a posicao do personagem e gera uma hitbox responsiva de acordo com a altura do boneco.
getPersonagemHitbox :: (Int, Int) -> Hitbox
getPersonagemHitbox (x, y) =
  let halfSize = 20
  in ((fromIntegral x*15- halfSize, fromIntegral y*15 - halfSize),
      (fromIntegral x*15 + halfSize, fromIntegral y*15 + halfSize))

---------------------------
----------- tarefa 2

-- | a funcao __estadoRessalto__ garante que todos os inimigos nascem com a propriedade Ressalta a True.
estadoRessalto :: Personagem -> Bool
estadoRessalto (Personagem _ _ _ _ _ _ ressaltaI _ _ _) = ressaltaI == True

-- | a funcao __posInimigos__ recebe como argumentos uma lista de inimigos e guarda as suas coordenadas (x,y) numa lista para mais tarde garantir
--   que o nosso jogador nao nasce no lugar de um inimigo (invalidando o mapa/perdendo automaticamente/bugando)
posInimigos :: [Personagem] -> [Posicao]
posInimigos inimigos = map (\(Personagem _ _ (x,y) _ _ _ _ _ _ _) -> (x,y)) inimigos

-- | A funcao __contaFantasmasVidas__ verifica que todos os inimigos do tipo = "Fantasma" tem 1 e 1 só vida.
contaFantasmasVidas :: [Personagem] -> [Personagem]
contaFantasmasVidas inimigos = filter (\(Personagem _ tipo _ _ _ _ _ vidas _ _) -> tipo == Fantasma && vidas == 1) inimigos

-- | A funcao __contaFantasmas__ verifica quantos inimigos do tipo = "Fantasma" existem na lista de inimigos fornecida.
contaFantasmas :: [Personagem] -> [Personagem]
contaFantasmas inimigos = filter (\(Personagem _ tipo _ _ _ _ _ _ _ _) -> tipo == Fantasma) inimigos

-- | A funcao __validarEscadas__ verifica se as escadas nao começam/terminam em alçapoes.
validarEscadas :: [[(Bloco, (Int, Int))]] -> Bool
validarEscadas [] = True
validarEscadas (linha:resto)
  | not (null linha) && temAlcapao linha = False
  | otherwise = validarLinhaEscada linha && validarEscadas resto

-- | funcao auxiliar de __validarEscadas__
temAlcapao :: [(Bloco, (Int, Int))] -> Bool
temAlcapao linha = any (\(bloco, _) -> bloco == Alcapao) linha

-- | funcao auxiliar de __validarEscadas__
temPlataforma :: [(Bloco, (Int, Int))] -> Bool
temPlataforma linha = any (\(bloco, _) -> bloco == Plataforma) linha

-- | funcao auxiliar de __validarEscadas__
validarLinhaEscada :: [(Bloco, (Int, Int))] -> Bool
validarLinhaEscada linha
  | tipoBloco (head linha) == Escada && tipoBloco (last linha) == Escada &&
    (temAlcapao linha || temAlcapao (init linha) || temAlcapao (tail linha)) &&
    (temPlataforma linha || temPlataforma (init linha) || temPlataforma (tail linha)) = False
  | otherwise = True
  where
    tipoBloco :: (Bloco, (Int, Int)) -> Bloco
    tipoBloco (bloco, _) = bloco

-----------------------------------

-- | Função recursiva para encontrar posições de blocos P e A
encontrarBlocosPA :: [[Bloco]] -> [(Double,Double)]
encontrarBlocosPA mapa = concatMap (\(x, y) -> if acharBloco2 (x, y) mapa `elem` [Plataforma, Alcapao] then [(x, y)] else []) todasAsPosicoes
    where
        todasAsPosicoes = [(fromIntegral x, fromIntegral y) | x <- [0..length (head mapa) - 1], y <- [0..length mapa - 1]]

-- | Função auxiliar para achar bloco numa posição no referencial (x,y) da matriz.
acharBloco2 :: Posicao -> [[Bloco]] -> Bloco
acharBloco2 (x, y) mapa = (mapa !! round y) !! round x

-- | A funcao __verificaSeExisteNaLista__ verifica se existem personagens e colecionaveis dentro de blocos do tipo Plataforma ou Alcapoes para evitar bugs.
-- casos de uso: garantir que o meu personagem nao nasce dentro de uma plataforma e nao se mexe.
--               garantir que todos os colecionaveis sao alcançáveis, isto é, nao estao "dentro" dos blocos.
verificaSeExisteNaLista :: [(Double,Double)] -> [[Bloco]] -> Bool
verificaSeExisteNaLista [] _= False
verificaSeExisteNaLista ((x,y):t) mapa = elem (x,y) (encontrarBlocosPA mapa) || verificaSeExisteNaLista t mapa

---------------------------
----------- tarefa 3

-- | a funcao __somarVetores__ é uma funcao básica de fisica que serve para somar 2 vetores do tipo velocidade (Double,Double).
somarVetores :: Velocidade -> Velocidade -> Velocidade
somarVetores (vx1, vy1) (vx2, vy2) = (vx1 + vx2, vy1 + vy2)

-- | a funcao __calcularDistancia__ recebe como argumento dois vetores/posicoes e calcula a distancia real entre eles.
calcularDistancia :: Velocidade -> Velocidade -> Double
calcularDistancia (x, y) (z, t) = sqrt $ (z - x)^2 + (t - y)^2
