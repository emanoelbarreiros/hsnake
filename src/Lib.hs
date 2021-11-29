module Lib where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

type Pos = (Int, Int)

data Direcao = Norte | Sul | Leste | Oeste | Parado deriving (Eq, Show)

data Modo = Inicio | Jogando | GameOver deriving (Eq, Show)

data Mundo = Estado {
    cobra :: Pos
  , direcao :: Direcao
  , contFrames :: Int
} deriving Show


mundoInicial :: Mundo
mundoInicial = Estado {
    cobra = (0,0)
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
desenhaMundo m = desenhaSegmento $ cobra m


tratarEvento :: Event -> Mundo -> Mundo
tratarEvento (EventKey (SpecialKey KeyUp) Down _ _) est@(Estado (x,y) _ _) = est {cobra = (x,y + 1)}
tratarEvento (EventKey (SpecialKey KeyDown) Down _ _) est@(Estado (x,y) _ _) = est {cobra = (x,y - 1)}
tratarEvento (EventKey (SpecialKey KeyLeft) Down _ _) est@(Estado (x,y) _ _) = est {cobra = (x - 1,y)}
tratarEvento (EventKey (SpecialKey KeyRight) Down _ _) est@(Estado (x,y) _ _) = est {cobra = (x + 1,y)}
tratarEvento _ est = est


atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo t est = est