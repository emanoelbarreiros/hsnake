module Main where

import Lib
import Graphics.Gloss

main :: IO ()
main = do
    play (InWindow "hSnake" (tamJanela , tamJanela) (500, 500)) white 60 mundoInicial desenhaMundo tratarEvento atualizaMundo
