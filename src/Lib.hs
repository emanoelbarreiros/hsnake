module Lib where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Random

type Pos = (Int, Int)

data Direcao = Norte | Sul | Leste | Oeste | Parado deriving (Eq, Show)

data Modo = Inicio | Jogando | GameOver deriving (Eq, Show)

data Mundo = Estado {
    cobra :: [Pos]
  , direcao :: Direcao
  , contFrames :: Int
  , randomGen :: StdGen
  , comida :: Pos
} deriving Show

(+>) :: Pos -> Direcao -> Pos
(x,y) +> Norte = (x, y + 1)
(x,y) +> Sul = (x, y - 1)
(x,y) +> Leste = (x + 1, y)
(x,y) +> Oeste = (x - 1, y)
(x,y) +> Parado = (x,y)

mundoInicial :: Mundo
mundoInicial = Estado {
    cobra = [(0,0), (0,-1), (0,-2)]
  , direcao = Parado
  , contFrames = 0
  , randomGen = mkStdGen 0
  , comida = (0,0)
}

linhas :: Num a => a
linhas = 39


limite :: (Num a, Integral a) => a
limite = linhas `div` 2


tamJanela :: Num a => a
tamJanela = tamSegmt * linhas


tamSegmt :: Num a => a
tamSegmt = 10


acaoFrames :: Num a => a
acaoFrames = 10


desenhaSegmento :: Pos -> Picture
desenhaSegmento (x,y) = translate (fromIntegral x * tamSegmt) (fromIntegral y * tamSegmt) $ rectangleSolid tamSegmt tamSegmt


desenhaMundo :: Mundo -> Picture
desenhaMundo m = pictures $ desenhaComida m : map desenhaSegmento (cobra m)

desenhaBola :: Mundo -> Picture
desenhaBola m = if bolaCriada m then uncurry translate (bola m) $ circleSolid 20 else Blank


tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyUp) Down _ _) m = m {direcao = novaDirecao (direcao m) Norte }
tratarEvento (EventKey (SpecialKey KeyDown) Down _ _) m = m {direcao = novaDirecao (direcao m) Sul }
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m {direcao = novaDirecao (direcao m) Oeste }
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m {direcao = novaDirecao (direcao m) Leste }
--tratarEvento (EventKey (MouseButton LeftButton) Down _ (x,y)) m = m {bola = (x,y), bolaCriada = True }
tratarEvento _ m = m


novaDirecao :: Direcao -> Direcao -> Direcao
novaDirecao Parado i = i
novaDirecao atual inten
    | atual == inten || atual == oposto inten = atual
    | otherwise = inten


oposto :: Direcao -> Direcao
oposto Norte = Sul
oposto Sul = Norte
oposto Leste = Oeste
oposto Oeste = Leste
oposto Parado = Parado

atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo _ est@(Estado cob dir fra rand com)
    | fra `mod` acaoFrames == 0 = Estado novaCobra dir (fra + 1) novoRandom novaComida
    | otherwise = est {contFrames = fra + 1}
    where
        novaCobra = atualizaCobra cob dir com
        (novaComida, novoRandom) = atualizaComida rand com novaCobra

atualizaComida :: StdGen -> Pos -> [Pos] -> (Pos, StdGen)
atualizaComida g com cob = if com == head cob
                           then novaComida cob g
                           else (com, g)

novaComida :: [Pos] -> StdGen -> (Pos, StdGen)
novaComida cob gen = if (x, y) `notElem` cob then ((x, y), stdGen3) else novaComida cob stdGen3
                     where
                       (x, stdGen2) = randomR (-limite, limite) gen
                       (y, stdGen3) = randomR (-limite, limite) stdGen2

atualizaCobra :: [Pos] -> Direcao -> Pos -> [Pos]
atualizaCobra cob Parado _ = cob
atualizaCobra cob d com
    | com == novaPosicao = novaPosicao : cob
    | otherwise = novaPosicao : init cob
    where
        novaCabeca = head cob +> d
        novaPosicao = reposicionaCabeca novaCabeca

movimentaCobra :: [Pos] -> Direcao -> [Pos]
movimentaCobra cob Parado = cob
movimentaCobra cob d = novaPosicao : init cob
                       where
                           novaCabeca = head cob +> d
                           novaPosicao = reposicionaCabeca novaCabeca

reposicionaCabeca :: Pos -> Pos
reposicionaCabeca (x,y)
    | x > limite = (-limite, y)
    | x < -limite = (limite, y)
    | y < -limite = (x, limite)
    | y > limite = (x, -limite)
    | otherwise = (x,y)


desenhaComida :: Mundo -> Picture
desenhaComida (Estado _ _ _ _ (x,y)) =
  translate (fromIntegral x * tamSegmt) (fromIntegral y * tamSegmt) $ color red $ circleSolid (tamSegmt / 2 + 1)