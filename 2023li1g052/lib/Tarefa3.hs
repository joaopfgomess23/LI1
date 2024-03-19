{- |
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Guilherme Frias Marques <a106805@alunos.uminho.pt>
              João Pedro Ferreira Gomes <a107377@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1
import Utils
import MapRelated

{- |
A funcao movimenta anima todos os personagens. Calcula as suas novas posicoes e respectivas consequencias com as açoes do jogo.
-}
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed time jogo@(Jogo ModoJogo skin mapa@(Mapa (posInicial,dir) pf listaBlocos) inimigos colecionaveis jogador@(Personagem jvel tipo (x, y) jdir tam emescada ressaltaJ vidas pontos (temMartelo, tempo))) =
    Jogo novoModoJogo skin novoMapa (novosInimigos inimigos jogador) (novosColecionaveis colecionaveis jogo) (atualizarJogador colecionaveis mapa jogador)
  where
    -- | a funcao __novosInimigos__ atualiza o estado do jogo verificando se a vida dos inimigos é >0 e caso contrario o mesmo é deixado de ser
    -- representado no mapa.
    novosInimigos :: [Personagem] -> Personagem -> [Personagem]
    novosInimigos inimigos jogador = filter (\inimigo@(Personagem _ _ _ _ _ _ _ vidainimigo _ _) -> vidainimigo > 0) (map (atualizarInimigo jogador) inimigos)

    novoMapa | estaSobreAlcapao mapa jogador && tipo == Jogador = Mapa (posInicial, dir) pf (substituirBlocoPorVazio (x+19, -y+8) listaBlocos)
             | otherwise = mapa

    novoModoJogo 
                 | ((x,y) == pf || calcularDistancia (x,y) pf < 2) && not (colisoesParede mapa jogador) = Ganhar
                 | vidas == 0 = Perder
                 | vidas > 0 && fst (posicaoSeColide inimigos temMartelo jogador posInicial) == (Jogo Colidir skin mapa inimigos colecionaveis jogador) = Colidir
                 | otherwise = ModoJogo

    -- | funcao auxiliar de __novosInimigos__ que faz as verificaçoes necessarias e atualiza a vida dos inimigos caso colidam com o jogador principal
    -- enquanto este ultimo tem martelo.
    atualizarInimigo :: Personagem -> Personagem -> Personagem
    atualizarInimigo jogador inimigo@(Personagem vel tipo (x,y) dir tam escada ressalta vidainimigo ponto dano)
        | colisoesPersonagens inimigo jogador && 
          temMartelo == True &&
          not (colisoesParede mapa inimigo) &&
          estaSobrePlataforma mapa inimigo = inimigo    { 
                                                        vida = vidainimigo - 1,
                                                        velocidade = somarVetores (velocidade inimigo) gravidade
                                                        }
        | otherwise = inimigo {velocidade = somarVetores (velocidade inimigo) gravidade
                                                        }

    -- | a funcao __posicaoSeColide__ verifica se o jogador colide com algum dos inimigos, e se isso acontecer o jogador perde e volta para
    --   a posicao onde se inicia o mapa, funciona como um restart game.
    posicaoSeColide :: [Personagem] -> Bool -> Personagem -> Posicao -> (Jogo,Posicao)
    posicaoSeColide inimigos martelo jogador@(Personagem _ _ _ dirjogador _ _ _ _ _ _) posInicial
        | any (\inimigo@(Personagem _ _ _ dirinimigo _ _ _ _ _ _) -> colisoesPersonagens jogador inimigo) inimigos && martelo == False && vidas > 1 = (Jogo Colidir skin mapa inimigos colecionaveis jogador,posInicial)
        | otherwise = (Jogo ModoJogo skin mapa inimigos colecionaveis jogador,posAtual)
        where
            posAtual = posicao jogador
    
    -- | A funcao __atualizarJogador__ é responsavel por atualizar as informaçoes do Jogador durante o jogo.
    -- Ex. Se um jogador nao estiver sobre uma plataforma esta funcao e responsavel por fazer aplicar o vetor gravidade na posiçao em que o mesmo
    --     se encontra.
    atualizarJogador :: [(Colecionavel,Posicao)] -> Mapa -> Personagem -> Personagem
    atualizarJogador listacolecionaveis mapa@(Mapa (posi,dirj) posf listablocos) jogador@(Personagem vel Jogador pos dir tam emEscada ressaltaJ vida pontos aplicadano@(temMartelo, tempo))
        | estaSobrePlataforma mapa jogador = jogador { 
                                                        posicao = snd $ posicaoSeColide inimigos temMartelo jogador posInicial,
                                                        vida = novaVida,
                                                        pontos = calcularPontos jogador listacolecionaveis pontos,
                                                        aplicaDano = if fst (atualizarDano jogador listacolecionaveis aplicadano) == True && tempo < 10 then 
                                                                        (True, (tempo + 1/20))
                                                                    else (False,0)
                                                    }
        -- caso o jogador nao esta sobre uma plataforma
        | otherwise = jogador {posicao = somarVetores (pos) (fst gravidade,-snd gravidade/10),
                               pontos = calcularPontos jogador listacolecionaveis pontos,
                               aplicaDano = if fst (atualizarDano jogador listacolecionaveis aplicadano) == True && tempo < 10 then 
                                                                        (True, (tempo + 1/20))
                                                                    else (False,0)}
    
    -- | a funcao __novaVida__ verifica se existe uma colisao entre um inimigo e o jogador e o mesmo esta desarmado, caso verdade é retirada 1 vida 
    -- ao jogador, caso contrário e retornado o parametro inicial vida.
    novaVida :: Int
    novaVida
        | (any (\inimigo -> colisoesPersonagens inimigo jogador) inimigos) && not temMartelo = vidas - 1
        | otherwise = vidas

    -- | a funcao __novosColecionaveis__ verifica se o jogador em alguma instancia do jogo se encontra na posicao de um colecionavel e atualiza a
    -- lista colecionaveis atual da sessao de jogo.
    novosColecionaveis :: [(Colecionavel, Posicao)] -> Jogo -> [(Colecionavel, Posicao)]
    novosColecionaveis colecionaveis jogo@(Jogo ModoJogo _ mapa listainimigos listacolecionaveis personagem@(Personagem _ Jogador (x,y) _ _ _ _ _ pontos aplicadano)) =
            [c | c <- colecionaveis, not (colisoesColecionavel c jogador && calcularDistancia (snd c) (x,y) < 2)]

-- | funcao que garante que o jogador esta na posicao do colecionavel.
colisoesColecionavel :: (Colecionavel, Posicao) -> Personagem -> Bool
colisoesColecionavel (colecionavel, (cx, cy)) jogador =
  let (px, py) = posicao jogador
      (tw, th) = tamanho jogador
      (cw, ch) = tamanhoColecionavel colecionavel
      offset = 0.75  -- offset para criar uma hitbox nos items
  in px < cx + cw + offset && px + tw > cx - offset && py < cy + ch + offset && py + th > cy - offset

-- | a funcao __calcularPontos__ verifica o numero de moedas apanhadas confirmando colisoes com colecionaveis. Caso estas sejam com colecionaveis
--   do tipo martelo, adiciona 100 aos pontos atuais do jogador para mais tarde poderem ser alterados atraves do conceito de records.
calcularPontos :: Personagem -> [(Colecionavel, Posicao)] -> Int -> Int
calcularPontos jogador colecionaveis pontosAtuais =
    let pontosNovos = foldr (\c acc -> if colisoesColecionavel c jogador && (calcularDistancia (snd c) (posicao jogador) < 2) then atualizarPontos (fst c) acc else acc) pontosAtuais colecionaveis
    in pontosNovos
    where
        -- | Função para atualizar os pontos com base no tipo de colecionável
        atualizarPontos :: Colecionavel -> Int -> Int
        atualizarPontos Moeda pontos = pontos + 100
        atualizarPontos _ pontos = pontos

-- | Função para atualizar o argumento aplicaDano com base nos colecionáveis
atualizarDano :: Personagem -> [(Colecionavel, Posicao)] -> (Bool, Double) -> (Bool, Double)
atualizarDano jogador colecionaveis aplicaDanoAtual =
    foldr (\c acc -> if colisoesColecionavel c jogador && calcularDistancia (snd c) (posicao jogador) < 2 then atualizarDanoColecionavel (fst c) acc else acc) aplicaDanoAtual colecionaveis
    where
        -- | Função auxiliar de __atualizarDano__.
        atualizarDanoColecionavel :: Colecionavel -> (Bool, Double) -> (Bool, Double)
        atualizarDanoColecionavel Martelo (_, tempo) = (True, 0)  -- se colidir com um martelo, ativa o dano por 10 segundos
        atualizarDanoColecionavel _ acc = acc
    
-- | Funcao auxiliar de __novosColecionaveis__ que atribui aos colecionaveis um tamanho, podendo ser considerado uma hitbox (Nao corresponde ao tamanho da matriz real).
tamanhoColecionavel :: Colecionavel -> (Double,Double)
tamanhoColecionavel Moeda = (1, 1)
tamanhoColecionavel Martelo = (1, 1)

-- | a funcao __estaSobrePlataforma__ verifica se um personagem esta por cima de um bloco do tipo Plataforma (se estiver numa escada tambem esta
--   sobre uma plataforma, visto que é um dos requisitos para que um mapa seja válido).
estaSobrePlataforma :: Mapa -> Personagem -> Bool
estaSobrePlataforma mapa@(Mapa (posi,dir) posf listablocos) (Personagem _ _ (x,y) _ tam _ _ _ _ _) =
    case  (acharBloco (round x,(round y)-2) (transformarMatriz listablocos constante)) of
        Plataforma -> True
        Escada -> True
        _ -> False
-- | a funcao __estaSobreAlcapao__ verifica se um personagem esta por cima de um bloco do tipo Alcapao. Retorna o valor booleano associado.
--   Em caso afirmativo retorna True, caso contrario retorna False.
estaSobreAlcapao :: Mapa -> Personagem -> Bool
estaSobreAlcapao mapa@(Mapa (posi,dir) posf listablocos) (Personagem _ tipo (x,y) _ tam _ _ _ _ _) =
    case  (acharBloco (round x,(round y)-2) (transformarMatriz listablocos constante)) of
        Alcapao -> True
        _ -> False

-- | A funcao __substituirBlocoPorVazio__ substitui um elemento de uma matriz de blocos (mapa). Dadas as coordenadas (x,y) do referencial esta
--   funcao trata de substituir o bloco presente nessa mesma posicao por um do tipo Vazio.
substituirBlocoPorVazio :: Posicao -> [[Bloco]] -> [[Bloco]]
substituirBlocoPorVazio (x, y) blocos = substituirElementoNaMatriz (round y) (round x) Vazio blocos

-- | funcao auxiliar de __substituirBlocoPorVazio__.
substituirElementoNaMatriz :: Int -> Int -> a -> [[a]] -> [[a]]
substituirElementoNaMatriz _ _ _ [] = []
substituirElementoNaMatriz 0 coluna novobloco (linha:resto) = substituirNaLinha coluna novobloco linha : resto
substituirElementoNaMatriz linha coluna novobloco (h:t) = h : substituirElementoNaMatriz (linha - 1) coluna novobloco t

-- | funcao auxiliar de __substituirBlocoPorVazio__.
substituirNaLinha :: Int -> a -> [a] -> [a]
substituirNaLinha _ _ [] = []
substituirNaLinha 0 novobloco (h:t) = novobloco : t
substituirNaLinha coluna novobloco (h:t) = h : substituirNaLinha (coluna - 1) novobloco t

{- | a funcao __direcaoOposta__ recebe uma direcao e retorna a sua oposta.

== Exemplo

>>>direcaoOposta Norte
Sul
-}
direcaoOposta :: Direcao -> Direcao
direcaoOposta dir =
    case dir of
        Norte -> Sul
        Sul -> Norte
        Este -> Oeste
        Oeste -> Este
