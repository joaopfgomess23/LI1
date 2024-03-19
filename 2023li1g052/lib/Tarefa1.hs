{- |
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import MapRelated
import Utils

{- |
A funcao colisoesParede recebe dois arguementos, __mapa__ e __personagem__, respetivamente, e retorna um valor __boleano__.
A sua funcao é testar se um personagem se encontra em colisao com algum dos limites do mapa (laterais ou topo) ou com algum bloco de P.
-}
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede mapa@(Mapa (posi,diri) posf listaBlocos) p@(Personagem vel tipo (x,y) dir tam escada ressalta vida ponto dano)
            | dentroDosLimites (x,y) &&
              not (acharBloco (round x,round y-1) (transformarMatriz listaBlocos constante) == Plataforma) = False
            | otherwise = True

{- |
A funcao __colisoesPersonagens__ recebe dois argumentos, ambos de Personagens e o seu objetivo é testar se dois personagens se encontram em colisao atráves do conceito aprimorado de hitbox.
Se o nosso jogador se encontra na posicao (x,y), esta funcao verifica se na posicao (x"+k",y"+k") dependendo da direção que o nosso player se move, se existe um inimigo, sendo k uma constante.
Caso exista um inimigo na direcao na qual o nosso player se move a funcao __colisoesPersonagens__ retorna True, caso contrário retorna False.
-}
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1@(Personagem vel1 tipo1 (x1,y1) dir1 tam1 escada1 ressalta1 vida1 ponto1 dano1) p2@(Personagem vel2 tipo2 (x2,y2) dir2 tam2 escada2 ressalta2 vida2 ponto2 dano2)
    | calcularDistancia (x1,y1) (x2,y2) < 1.5 = vaoColidir dir1 dir2 || colideHitBoxes (getPersonagemHitbox (round x1,round y1)) dir1 (getPersonagemHitbox (round x2, round y2)) dir2
    | otherwise = calcularDistancia (x1,y1) (x2,y2) < 2 && colideHitBoxes (getPersonagemHitbox (round x1,round y1)) dir1 (getPersonagemHitbox (round x2, round y2)) dir2
