module Main where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Data.Maybe
import MapRelated
import Utils
import GHC.Float (double2Float)
import Mapas

-- | a funcao __drawOptionsMainMenu__ desenha as opcoes do main menu
drawOptionsMainMenu op =
  case op of

    -- main menu
    Jogar -> Pictures [Translate (-50) 10 $ Color red $ boldText "Jogar",
                       Translate (-50) (-70) $ Color white $ boldText "Editar",
                       Translate (-90) 30 $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                       Translate (-50) (-150) $ color white $ boldText "Ajuda",
                       Translate (-50) (-230) $ color white $ boldText "Sair"]

    Ajuda -> Pictures [Translate (-50) 10 $ Color white $ boldText "Jogar",
                           Translate (-50) (-70) $ Color white $ boldText "Editar",
                           Translate (-90) (-130) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                           Translate (-50) (-150) $ color red $ boldText "Ajuda",
                           Translate (-50) (-230) $ color white $ boldText "Sair"]

    Editar -> Pictures [Translate (-50) 10 $ color white $ boldText "Jogar",
                        Translate (-50) (-70) $ Color red $ boldText "Editar",
                        Translate (-50) (-150) $ Color white $ boldText "Ajuda",
                        Translate (-90) (-50) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-30) $ Color white $ boldText "Skin",
                        Translate (155) (-90) $ Color white $ boldText "Mapa",
                        Translate (155) (-160) $ Color white $ boldText "Voltar",
                        Translate (-50) (-230) $ color white $ boldText "Sair"]

    EditarSubmenu Skin -> Pictures [Translate (-50) 10 $ color white $ boldText "Jogar",
                        Translate (-50) (-70) $ Color red $ boldText "Editar",
                        Translate (-50) (-150) $ Color white $ boldText "Ajuda",
                        Translate (-90) (-50) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-30) $ Color red $ boldText "Skin",
                        Translate (135) (-5) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-90) $ Color white $ boldText "Mapa",
                        Translate (155) (-160) $ Color white $ boldText "Voltar",
                        Translate (-50) (-230) $ color white $ boldText "Sair"]

    EditarSubmenu MapSkin -> Pictures [Translate (-50) 10 $ color white $ boldText "Jogar",
                        Translate (-50) (-70) $ Color red $ boldText "Editar",
                        Translate (-50) (-150) $ Color white $ boldText "Ajuda",
                        Translate (-90) (-50) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-30) $ Color white $ boldText "Skin",
                        Translate (155) (-90) $ Color red $ boldText "Mapa",
                        Translate (135) (-65) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-160) $ Color white $ boldText "Voltar",
                        Translate (-50) (-230) $ color white $ boldText "Sair"]

    EditarSubmenu Voltar -> Pictures [Translate (-50) 10 $ color white $ boldText "Jogar",
                        Translate (-50) (-70) $ Color red $ boldText "Editar",
                        Translate (-50) (-150) $ Color white $ boldText "Ajuda",
                        Translate (-90) (-50) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (155) (-30) $ Color white $ boldText "Skin",
                        Translate (155) (-90) $ Color white $ boldText "Mapa",
                        Translate (155) (-160) $ Color red $ boldText "Voltar",
                        Translate (135) (-135) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                        Translate (-50) (-230) $ color white $ boldText "Sair"]

    Sair ->  Pictures [Translate (-50) 10 $ color white $ boldText "Jogar",
                       Translate (-50) (-70) $ Color white $ boldText "Editar",
                       Translate (-50) (-150) $ color white $ boldText "Ajuda",
                       Translate (-50) (-230) $ color red $ boldText "Sair",
                       Translate (-90) (-210) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10]

-- | A funcao __drawOptionsPauseMenu__ desenha as opcoes do pause menu
drawOptionsPauseMenu op =
  case op of
    -- pause menu
    ContinuarJogo -> Pictures [Translate (-50) 10 $ Color red $ boldText "Continuar",
                              Translate (-90) 30 $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10,
                              Translate (-50) (-70) $ color white $ boldText "Sair"]
    
    SairModoJogo ->  Pictures [Translate (-50) 10 $ color white $ boldText "Continuar",
                       Translate (-50) (-70) $ Color red $ boldText "Sair",
                       Translate (-90) (-50) $ scale 2.3 2.3 $ color green $ rotate 45 $ rectangleSolid 10 10]

-- constante para nao estar a alterar as coordenadas 1 a 1 (altera apenas o y)
k =50
drawOptionsSkinMenu op images =
      case op of
      
      Mario -> Pictures [Translate (-475) (180+k) $ color (light $ light $ red) $ rectangleSolid 250 300,
                           Translate (-475) (180+k) $ scale 7.5 7.5 $ fromJust (lookup "Mario" images), 
                           Translate (-550) (-40+k) $ Color red $ boldText "Mario",
                           Translate (-25) (180+k) $ color (yellow) $ rectangleSolid 250 300,
                           Translate (-25) (180+k) $ scale 6.5 6.5 $ fromJust (lookup "Sonic" images), 
                           Translate (-100) (-40+k) $ Color white $ boldText "Sonic",
                           Translate (425) (180+k) $ color (green) $ rectangleSolid 250 300,
                           Translate (425) (190+k) $ scale 7 7 $ fromJust (lookup "Zombie" images), 
                           Translate (320) (-40+k) $ color white $ boldText "Zombie",
                           Translate (-100) (-475+k) $ Color white $ boldText "Voltar"]

      Sonic -> Pictures [Translate (-475) (180+k) $ color (red) $ rectangleSolid 250 300,
                          Translate (-475) (180+k) $ scale 7.5 7.5 $ fromJust (lookup "Mario" images), 
                           Translate (-550) (-40+k) $ Color white $ boldText "Mario",
                           Translate (-25) (180+k) $ color (light $ light $ yellow) $ rectangleSolid 250 300,
                           Translate (-25) (180+k) $ scale 6.5 6.5 $ fromJust (lookup "Sonic" images), 
                           Translate (-100) (-40+k) $ Color yellow $ boldText "Sonic",
                           Translate (425) (180+k) $ color (green) $ rectangleSolid 250 300,
                           Translate (425) (190+k) $ scale 7 7 $ fromJust (lookup "Zombie" images), 
                           Translate (320) (-40+k) $ color white $ boldText "Zombie",
                           Translate (-100) (-475+k) $ Color white $ boldText "Voltar"]
      
      Zombie -> Pictures [Translate (-475) (180+k) $ color (red) $ rectangleSolid 250 300,
                              Translate (-475) (180+k) $ scale 7.5 7.5 $ fromJust (lookup "Mario" images), 
                           Translate (-550) (-40+k) $ Color white $ boldText "Mario",
                           Translate (-25) (180+k) $ color (yellow) $ rectangleSolid 250 300,
                           Translate (-25) (180+k) $ scale 6.5 6.5 $ fromJust (lookup "Sonic" images), 
                           Translate (-100) (-40+k) $ Color white $ boldText "Sonic",
                           Translate (425) (180+k) $ color (light $ light $ green) $ rectangleSolid 250 300,
                           Translate (425) (190+k) $ scale 7 7 $ fromJust (lookup "Zombie" images), 
                           Translate (320) (-40+k) $ color green $ boldText "Zombie",
                           Translate (-100) (-475+k) $ Color white $ boldText "Voltar"]
      VoltarSkins -> Pictures [Translate (-475) (180+k) $ color (red) $ rectangleSolid 250 300,
                              Translate (-475) (180+k) $ scale 7.5 7.5 $ fromJust (lookup "Mario" images), 
                           Translate (-550) (-40+k) $ Color white $ boldText "Mario",
                           Translate (-25) (180+k) $ color (yellow) $ rectangleSolid 250 300,
                           Translate (-25) (180+k) $ scale 6.5 6.5 $ fromJust (lookup "Sonic" images), 
                           Translate (-100) (-40+k) $ Color white $ boldText "Sonic",
                           Translate (425) (180+k) $ color (green) $ rectangleSolid 250 300,
                           Translate (425) (190+k) $ scale 7 7 $ fromJust (lookup "Zombie" images), 
                           Translate (320) (-40+k) $ color white $ boldText "Zombie",
                           Translate (-100) (-475+k) $ Color red $ boldText "Voltar"]

-- | a funcao __boldText__ da um efeito de carregado ao texto que é passado como argumento. Sao adicionadas letras sobre letras com um pequeno
--  offset, o que da uma sensacao visual de carregado.
boldText :: String -> Picture
boldText text = Pictures [textAt (1.5, 0) text, textAt (-1.5, 0) text, textAt (0, 1.5) text, textAt (0, -1.5) text, textAt (0, 0) text,
                          textAt (2, 0) text, textAt (-2, 0) text, textAt (0, 2) text, textAt (0, -2) text, textAt (0, 0) text,
                          textAt (2.5, 0) text, textAt (-2.5, 0) text, textAt (0, 2.5) text, textAt (0, -2.5) text, textAt (0, 0) text]
textAt :: (Float, Float) -> String -> Picture
textAt (offsetX, offsetY) text = Translate offsetX offsetY (Scale 0.5 0.5 (Text text))

-- | A funcao __drawState__ desenha no ecra todos os elementos graficos que sao mostrados no nosso jogo.
-- main menu
drawState :: Imagens -> Jogo -> Picture
drawState images (Jogo (Opcoes op) skin _ _ _ _) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                        drawOptionsMainMenu op,
                                                        Translate (-600) (350) $ scale 3.35 3.35 $ fromJust $ lookup "Highscore" images,
                                                        Translate (-560) 345 $ scale 0.5 0.5 $ color yellow $ boldText "Topscore: ",
                                                        Translate (-635) (60) $ scale 0.5 0.5 $ color white $ boldText "Skin atual:",
                                                        Translate (-560) (-50) $ color cor $ rectangleSolid 200 200,
                                                        Translate (-560) (-50) $ scale 4.5 4.5 $ fromJust $ lookup (show skin) images]
                                                   where cor = if skin == Mario then light (light red)
                                                                  else if skin == Zombie then green 
                                                                         else yellow
-- help menu
drawState images (Jogo (MenuAjuda) _ _ _ _ _) = pictures [scale 1 1 $ fromJust $ lookup "Ajuda" images,
                                                      Translate (-90) (400) $ scale 0.5 0.5 $ color yellow $ boldText "Movimentos",
                                                        Translate (-50) (-400) $ scale 0.5 0.5 $ color red $ boldText "Voltar",
                                                         Translate (-520) (350) $ scale 0.5 0.5 $ color white $ boldText "Andar para cima",
                                                         Translate (-520) (100) $ scale 0.5 0.5 $ color white $ boldText "Andar para a direita",
                                                         Translate (-520) (-130) $ scale 0.5 0.5 $ color white $ boldText "Andar para a esquerda",
                                                         Translate (-520) (-390) $ scale 0.5 0.5 $ color white $ boldText "Andar para baixo",
                                                         Translate (220) (100) $ scale 0.5 0.5 $ color white $ boldText "Saltar",
                                                         Translate (200) (-200) $ scale 0.5 0.5 $ color white $ boldText "Pausar jogo"]
-- skin menu
drawState images (Jogo (MenuSkin op) _ _ _ _ _) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                        drawOptionsSkinMenu op images]
-- lose menu
drawState images (Jogo (Perder) _ _ _ _ personagem) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                      Translate (0) 0 $ scale 0.5 0.5 $ color yellow $ boldText $ "Ficaste sem vidas"]
-- win menu
drawState images (Jogo (Ganhar) _ _ _ _ personagem) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                      Translate (0) 0 $ scale 0.5 0.5 $ color yellow $ boldText $ "Score: " ++ show (pontos personagem)]
-- colidir menu
drawState images (Jogo (Colidir) _ _ _ _ personagem) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                      Translate (0) 0 $ scale 0.5 0.5 $ color orange $ boldText $ "Morreste"]
-- pausar menu
drawState images (Jogo (Pausar op) _ _ _ _ personagem) = pictures [scale 1 1 $ fromJust $ lookup "Background" images,
                                                                drawOptionsPauseMenu op]
-- modo jogo
drawState images jogo@(Jogo ModoJogo skin mapa@(Mapa (p,d) (xf,yf) blocos) inimigos colecionaveis personagem@(Personagem vel tipo (x,y) dir tam escada ressalta vida ponto (temmartelo,tempo))) =
  Pictures [ Color black $ rectangleSolid 600 400, -- background preto
             desenhaMapa (jogo,images) (transformarMatriz blocos constante), -- desenha o mapa principal
             desenhaPersonagem, -- desenha personagem princiapl
             desenhaEstrela, -- desenha posicao final do map
             Pictures ( map desenhaPersonagemInimigo inimigos), -- desenha os inimigos
             Pictures ( map (\(colecionavel, posicao) -> desenhaColecionaveis colecionavel posicao) colecionaveis), -- desenha todos os elementos da lista coleciovais
             Translate ((realToFrac x-0.12)*15) ((realToFrac y-1.5)*15) $ desenhaTempoMartelo temmartelo tempo, -- desenha um retangulo com o resto do martelo
             Translate ((realToFrac x)*15) ((realToFrac y)*15) $ desenhaTemMartelo temmartelo tempo dir images, -- da um efeito visual de martelar
             --desenharCoordenadas (round x,round y), -- debug function
             --desenharCenas (round x,round y) mapa personagem, -- debug function
             --pictures $ map (\inimigo@(Personagem _ _ (xi,yi) _ _ _ ressaltai _ _ _) -> Translate (realToFrac xi*15) (realToFrac yi*15) $ scale 0.5 0.5 $ color white $ Text $ show $ ressaltai) inimigos,
             --color green $ drawHitbox (getPersonagemHitbox (round x,round y)), -- debug function hitbox personagem
             --color yellow $ drawHitboxFrente (getPersonagemHitbox (round x,round y)) dir, -- debug function hitbox a frente do personagem
             Translate (-19*15) (11.5*15) $ Color white $ Scale 0.13 0.13 $ Text $ "Donkey Kong Game v0.0.1 | Score: " ++ show ponto, -- info do jogo
             coracoes (-18.5) -- numero de vidas
           ]

           where
              desenhaPersonagem = Translate (realToFrac x*15) (realToFrac y*15) $ personagem_skin
              desenhaEstrela = Translate (realToFrac xf*15) (realToFrac yf*15) $ fromJust $ lookup "Estrela" images

              personagem_skin = case skin of
                        Mario -> case dir of
                                    Oeste -> scale (-1) 1 $ fromJust $ lookup "Mario" images
                                    Este -> fromJust $ lookup "Mario" images
                                    Norte -> scale (-1) 1 $ fromJust $ lookup "Mario" images
                                    Sul -> fromJust $ lookup "Mario" images
                        Sonic -> case dir of 
                                    Oeste -> scale (-1) 1 $ fromJust $ lookup "Sonic" images
                                    Este -> fromJust $ lookup "Sonic" images
                                    Norte -> scale (-1) 1 $ fromJust $ lookup "Sonic" images
                                    Sul -> fromJust $ lookup "Sonic" images
                        Zombie -> case dir of 
                                    Oeste -> scale (-1) 1 $ fromJust $ lookup "Zombie" images
                                    Este -> fromJust $ lookup "Zombie" images
                                    Norte -> scale (-1) 1 $ fromJust $ lookup "Zombie" images
                                    Sul -> fromJust $ lookup "Zombie" images

              coracoes xinicial = desenhaCoracoes xinicial vida

              desenhaCoracoes :: Float -> Int -> Picture
              desenhaCoracoes xinicial x | x>0 = pictures [Translate ((xinicial)*15) (10.6*15) $ scale 0.45 0.45 $ fromJust (lookup "Coracao" images), desenhaCoracoes (xinicial+1) (x-1)]
                                | otherwise = blank
              
              desenhaPersonagemInimigo :: Personagem -> Picture
              desenhaPersonagemInimigo (Personagem _ Fantasma (x,y) _ _ _ _ _ _ _) = Translate (realToFrac x*15) (realToFrac y*15) $ scale 0.4 0.8 $ fromJust (lookup "Fantasma" images)
              desenhaPersonagemInimigo (Personagem _ MacacoMalvado (x,y) _ _ _ _ _ _ _) = Translate (realToFrac x*15) (realToFrac y*15) $ scale 0.4 0.4 $ fromJust (lookup "Macaco" images)

              desenhaColecionaveis :: Colecionavel -> Posicao -> Picture
              desenhaColecionaveis tipo (x,y) = Translate (realToFrac x*15) (realToFrac y*15) $ scale 0.4 0.8 $ fromJust (lookup tipo_colecionavel images)
                      where tipo_colecionavel = case tipo of
                                Martelo -> "Martelo"
                                Moeda -> "Moeda"

              -- | a funcao __desenhaTempoMartelo__ mostra o tempo restante que o jogador tem para usar o martelo.
              desenhaTempoMartelo :: Bool -> Double -> Picture
              desenhaTempoMartelo temmartelo tempo
                | temmartelo && tempo < 5 = Pictures [scale 0.4 0.4 $ color white $ rectangleWire 100 10,
                                        scale 0.4 0.4 $ Color green $ rectangleSolid ((double2Float tempo * 10)*0.9) 10,
                                          Translate (-3.45*15) (-0.3*15) $ scale 0.2 0.2 $ color white $ boldText $ show (round tempo) ++ "/10"]
                | temmartelo && tempo >= 5 && tempo < 7.5 = Pictures [scale 0.4 0.4 $ color white $ rectangleWire 100 10,
                                        scale 0.4 0.4 $ Color yellow $ rectangleSolid ((double2Float tempo * 10)*0.9) 10,
                                        Translate (-3.45*15) (-0.3*15) $ scale 0.2 0.2 $ color white $ boldText $ show (round tempo) ++ "/10"]
                | temmartelo && tempo >= 7.5 = Pictures [scale 0.4 0.4 $ color white $ rectangleWire 100 10,
                                        scale 0.4 0.4 $ Color red $ rectangleSolid ((double2Float tempo * 10)*0.9) 10,
                                        Translate (-3.45*15) (-0.3*15) $ scale 0.2 0.2 $ color white $ boldText $ show (round tempo) ++ "/10"]
                | otherwise  = Blank

             -- | a funcao __desenhaTemMartelo__ faz a animaçao de "martelar".
              desenhaTemMartelo :: Bool -> Double -> Direcao -> Imagens -> Picture
              desenhaTemMartelo temmartelo tempo dir images
                | temmartelo && (dir == Oeste || dir == Norte) && odd (round tempo) = Pictures [Translate (-1*15) (1.3*15) $ scale 0.4 0.8 $ fromJust $ lookup "Martelo" images]
                | temmartelo && (dir == Oeste || dir == Norte) && even (round tempo) = Pictures [Translate (-1.5*15) (0.2*15)$ rotate (-45) $ scale 0.4 0.8 $ fromJust $ lookup "Martelo" images]
                | temmartelo && (dir == Este || dir == Sul) && odd (round tempo)= Pictures [scale (-1) 1 $ Translate (-1*15) (1.3*15) $ scale 0.4 0.8 $ fromJust $ lookup "Martelo" images]
                | temmartelo && (dir == Este || dir == Sul) && even (round tempo)= Pictures [scale (-1) 1 $ Translate (-1.5*15) (0.2*15) $ rotate (-45) $ scale 0.4 0.8 $ fromJust $ lookup "Martelo" images]
                | otherwise  = Blank
  --------------------------------------------------------------
-- debug functions
desenharCoordenadas :: (Int,Int) -> Picture
desenharCoordenadas (x,y) = Translate (fromIntegral x*15) (fromIntegral y*15) $ scale 0.1 0.1 $ color white $  Text ("(" ++ show x ++ ", " ++ show y ++ ")")

desenharCenas :: (Int,Int) -> Mapa -> Personagem -> Picture -- Direcao -> [[Bloco]] -> Picture
desenharCenas (x,y) mapa jogador@(Personagem _ _ _ _ _ _ ressalta _ _ aplicadano) = Translate (fromIntegral x*15) (fromIntegral y*15) $ scale 0.5 0.5 $ color white $ Text $ show $ ressalta

blocoToString :: Bloco -> String
blocoToString bloco = case bloco of
  Plataforma -> "P"
  Escada -> "E"
  Vazio -> "V"
  Alcapao -> "A"

boolToString :: Bool -> String
boolToString True  = "True"
boolToString False = "False"

-------------------------------------------------------------- debug section


gameEvent :: Event -> Jogo -> Jogo
------------------------------------------------------- opcoes menu
------------------------ mudar opcao
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes Jogar) _ mapa inimigos colecionaveis jogador) =
      let (novoMapa, novosInimigos, novosColecionaveis, novoJogador) = extrairJogoInicial (nivel1)
      in e { menu = ModoJogo, mapa = novoMapa, inimigos = novosInimigos, colecionaveis = novosColecionaveis, jogador = novoJogador }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes Jogar) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Sair }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes Jogar) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (Editar) }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes (Editar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Jogar }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes (Editar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Ajuda }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes (Editar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin) }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Skin)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu MapSkin) }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes (EditarSubmenu MapSkin)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Voltar) }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Voltar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin) }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Skin)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Voltar) }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Voltar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu MapSkin) }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes (EditarSubmenu MapSkin))_ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin) }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Voltar)) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (Editar) }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes (EditarSubmenu Skin)) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Mario}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (MenuSkin Mario) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin), skin = Mario}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (MenuSkin Sonic) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin), skin = Sonic}
gameEvent (EventKey (SpecialKey KeyRight) Down _ _) e@(Jogo (MenuSkin Mario) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Sonic}
gameEvent (EventKey (SpecialKey KeyLeft) Down _ _) e@(Jogo (MenuSkin Sonic) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Mario}
gameEvent (EventKey (SpecialKey KeyRight) Down _ _) e@(Jogo (MenuSkin Sonic) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Zombie}
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (MenuSkin Sonic) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin VoltarSkins}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (MenuSkin VoltarSkins) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin)}
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (MenuSkin VoltarSkins) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Sonic}
gameEvent (EventKey (SpecialKey KeyLeft) Down _ _) e@(Jogo (MenuSkin Zombie) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Sonic}
gameEvent (EventKey (SpecialKey KeyRight) Down _ _) e@(Jogo (MenuSkin Zombie) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Mario}
gameEvent (EventKey (SpecialKey KeyLeft) Down _ _) e@(Jogo (MenuSkin Mario) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuSkin Zombie}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (MenuSkin Zombie) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (EditarSubmenu Skin), skin=Zombie}
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes Ajuda) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Sair }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes Ajuda) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes (Editar) }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes Ajuda) _ mapa inimigos colecionaveis jogador) =
      e { menu = MenuAjuda }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (MenuAjuda) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Ajuda }
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Opcoes Sair) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Ajuda }
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Opcoes Sair) _ mapa inimigos colecionaveis jogador) =
      e { menu = Opcoes Jogar }
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Opcoes Sair) _ mapa inimigos colecionaveis jogador) =
      error "session killed"

-- caso perca 1 vida/colida com um inimigo e esteja desarmado
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo Colidir skin mapa inimigos colecionaveis jogador) =
      let (novoMapa, novosInimigos, novosColecionaveis, novoJogador) = extrairJogoInicial (nivel1)
      in e { mapa= novoMapa, menu = ModoJogo, inimigos = novosInimigos, colecionaveis = novosColecionaveis, jogador = novoJogador}

-- caso o jogador ganhe
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo Ganhar skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Opcoes Jogar,
          jogador = jogador {posicao = posi, pontos = 0, vida = 3, direcao = dir}
        }

-- caso perca completamente o jogo
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo Perder skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Opcoes Jogar,
          colecionaveis = colecionaveis,
          jogador = jogador { vida = 3, pontos = 0, posicao = posi, direcao = dir}
        }

-- menu de pausa [M]
gameEvent (EventKey (Char 'm') Down _ _) e@(Jogo ModoJogo skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar ContinuarJogo}
gameEvent (EventKey (Char 'M') Down _ _) e@(Jogo ModoJogo skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar ContinuarJogo}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Pausar ContinuarJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = ModoJogo}
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Pausar ContinuarJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar SairModoJogo}
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Pausar ContinuarJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar SairModoJogo}
gameEvent (EventKey (SpecialKey KeyUp) Down _ _) e@(Jogo (Pausar SairModoJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar ContinuarJogo}
gameEvent (EventKey (SpecialKey KeyDown) Down _ _) e@(Jogo (Pausar SairModoJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Pausar ContinuarJogo}
gameEvent (EventKey (SpecialKey KeyEnter) Down _ _) e@(Jogo (Pausar SairModoJogo) skin mapa@(Mapa (posi, dir) pf listablocos) inimigos colecionaveis jogador) =
      e { menu = Opcoes Jogar}

-- presiona tecla para jogar
gameEvent (EventKey k Down _ _) (Jogo ModoJogo skin mapa inimigos colecionaveis personagem@(Personagem vel tipo (x,y) dir tam escada ressalta vida ponto dano)) =
      Jogo ModoJogo skin mapa inimigos colecionaveis (atualizaJogador mapa (personagem {direcao = getDirecao k vel}) (Just (getAcao k)))
gameEvent (EventKey _ Up _ _) (Jogo ModoJogo skin mapa inimigos colecionaveis personagem@(Personagem vel tipo (x,y) dir tam escada ressalta vida ponto dano)) =
      Jogo ModoJogo skin mapa inimigos colecionaveis (atualizaJogador mapa personagem (Just Parar))
gameEvent _ j = j


 -- | A funcao __getDirecao__ transforma a tecla pressionada em direção para que o estado do boneco seja atualizado em conjunto com a funcao __gameEvent__.
getDirecao :: Key -> Velocidade -> Direcao
getDirecao (SpecialKey KeyUp) _ = Norte
getDirecao (SpecialKey KeyDown) _ = Sul
getDirecao (SpecialKey KeyLeft) _ = Oeste
getDirecao (SpecialKey KeyRight) _ = Este
getDirecao (SpecialKey KeySpace) vel = getDirecao (SpecialKey KeyLeft) vel

seed = geraAleatorios 10 1 !! 0

atualizaSeed :: Int -> Mapa -> [Personagem] -> Int
atualizaSeed x mapa inimigos | any (==True) (map (\inimigo -> colisoesParede mapa inimigo) inimigos) = x-1
                              | otherwise = x

gameTime :: Float -> Jogo -> Jogo
gameTime time e@(Jogo ModoJogo skin mapa inimigos colecionaveis personagem) =
              let
                  newseed = atualizaSeed seed mapa inimigos
                  listaMovimentos = [AndarDireita, AndarDireita,AndarDireita]

                  listaatualizada = if odd newseed then map (\acao -> Just $ acaocontraria acao) listaMovimentos
                                    else map (\acao -> Just acao) listaMovimentos
                                    
               in atualiza listaatualizada (Just Parar) $ movimenta (newseed) (realToFrac time) (Jogo ModoJogo skin mapa inimigos colecionaveis personagem)

gameTime time j = j

carregarImagens :: IO Imagens
carregarImagens = do
               background <- loadBMP "../2023li1g052/images/background.bmp"
               highscore <- loadBMP "../2023li1g052/images/highscore.bmp"
               mario <- loadBMP "../2023li1g052/images/mario.bmp"
               sonic <- loadBMP "../2023li1g052/images/sonic.bmp"
               zombie <- loadBMP "../2023li1g052/images/zombie.bmp"
               plataforma <- loadBMP "../2023li1g052/images/plataforma.bmp"
               escada <- loadBMP "../2023li1g052/images/escada.bmp"
               alcapao <- loadBMP "../2023li1g052/images/alcapao.bmp"
               moeda <- loadBMP "../2023li1g052/images/moeda.bmp"
               coracao <- loadBMP "../2023li1g052/images/coracao.bmp"
               fantasma <- loadBMP "../2023li1g052/images/fantasma.bmp"
               martelo <- loadBMP "../2023li1g052/images/martelo.bmp"
               estrela <- loadBMP "../2023li1g052/images/estrela.bmp"
               ajuda <- loadBMP "../2023li1g052/images/fundoAjuda.bmp"
               macaco <- loadBMP "../2023li1g052/images/macacomalvado.bmp"

               let imagens = [("Background", background),
                              ("Ajuda", ajuda),
                              ("Highscore", scale 0.1 0.1 highscore),
                              ("Mario",(scale 0.07 0.07 mario)),
                              ("Sonic", scale 0.035 0.035 sonic),
                              ("Zombie", scale 0.1 0.1 zombie),
                              ("Plataforma", scale 0.5 0.5 plataforma),
                              ("Escada", scale 0.5 0.5 escada),
                              ("Alcapao", scale 0.45 0.45 alcapao),
                              ("Moeda", scale 0.06 0.03 moeda),
                              ("Coracao", scale 0.03 0.03 coracao),
                              ("Fantasma", scale 0.8 0.8 fantasma),
                              ("Martelo", scale 0.13 0.07 martelo),
                              ("Estrela", scale 0.1 0.1 estrela),
                              ("Vazio", blank),
                              ("Macaco",scale 0.5 0.5 $ macaco)
                              ]
               return imagens

main :: IO ()
main = do
    images <- carregarImagens
    play FullScreen 
      orange
      20
      (fst nivel1)
      (drawState images)
      gameEvent
      gameTime


mapa2 =Mapa ((-18, -10), Este) (-19, -4)
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
