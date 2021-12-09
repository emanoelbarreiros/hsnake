module Main where

import Lib
import Graphics.Gloss
import System.Random

main :: IO ()
main = do
    let mundo = mundoInicial 
    stdGen <- getStdGen 
    play (InWindow "hSnake" (tamJanela , tamJanela) (500, 500)) white 60 (mundo { randomGen = stdGen }) desenhaMundo tratarEvento atualizaMundo
