module Lib where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

type Pos = (Int, Int)

data Direcao = Norte | Sul | Leste | Oeste | Parado deriving (Eq, Show)

data Modo = Inicio | Jogando | GameOver deriving (Eq, Show)

data Mundo = Estado {
    cobra :: [Pos]
  , direcao :: Direcao
  , contFrames :: Int
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
desenhaMundo m = pictures $ map desenhaSegmento (cobra m)


tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyUp) Down _ _) m = m {direcao = novaDirecao (direcao m) Norte }
tratarEvento (EventKey (SpecialKey KeyDown) Down _ _) m = m {direcao = novaDirecao (direcao m) Sul }
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m = m {direcao = novaDirecao (direcao m) Oeste }
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) m = m {direcao = novaDirecao (direcao m) Leste }
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
atualizaMundo _ est@(Estado cob dir fra )
    | fra `mod` acaoFrames == 0 = Estado novaCobra dir (fra + 1)
    | otherwise = est {contFrames = fra + 1}
    where
        novaCobra = movimentaCobra cob dir


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