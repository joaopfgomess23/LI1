{- |
Module      : Tarefa2
Description : Valida jogo
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1
import MapRelated
import Utils

{-|
A funcao valida recebe um argumento do tipo __Jogo__ e retorna um valor __boleano__. O objetido desta funcao é verificar se o mapa contido no jogo é valido através das seguintes regras:

    * 1. O mapa tem chao, uma P que impede que o jogador ou outro personagem caia fora do mapa.
    * 2. Todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.
    * 3. A posicao inicial de um jogador nao pode colidir com a posicao inicial de um outro personagem. Note que as posicoes iniciais de inimigos podem colidir entre estes.
    * 4. Número minimo de inimigos: 2 (dois.)
    * 5. Inimigos Fantasma têm exactamente 1 (uma) vida.
    * 6. Escadas não podem comecar/terminar em alcapoes, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma.
    * 7. Alcapoes nao podem ser menos largos que o jogador. (Feito para a escala dos blocos predefinidos (15x15)).
    * 8. Nao podem existir personagens nem colecionaveis “dentro” de plataformas ou alcapoes.
-}
valida :: Jogo -> Bool
valida (Jogo _ _ (mapa@(Mapa ((xinicial,yinicial),dir) posf listaBlocos)) inimigos colecionaveis jogador@(Personagem _ _ (x,y) _ tam@(xtam,ytam) _ ressaltaJ _ _ _)) 
                | (all (==True) (map (\x -> x == Plataforma) (last listaBlocos))) && -- 1
                  all estadoRessalto inimigos && -- 2
                  ressaltaJ == False && -- 2
                  not (elem (x,y) (posInimigos inimigos)) && -- 3
                  length inimigos >= 2 && -- 4
                  validarEscadas (transformarMatriz listaBlocos constante) && -- 6
                  xtam < 1*15 && -- 7
                  not (elem (x,y) (encontrarBlocosPA listaBlocos)) && -- 8
                  not (verificaSeExisteNaLista (map posicao inimigos) listaBlocos) && -- 8
                  not (verificaSeExisteNaLista posicoesColecionaveis listaBlocos) && -- 8
                  length (contaFantasmasVidas inimigos) == length (contaFantasmas inimigos) = True -- 5
                | otherwise = False
            where
                posicoesColecionaveis = map (\(_, pos) -> pos) colecionaveis
