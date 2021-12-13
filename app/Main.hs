module Main where

import Lib
import Graphics.Gloss
import System.Random

main :: IO ()
main = do
    let mundo = mundoInicial 
    stdGen <- getStdGen 
    let ((x,y), gen2) = novaComida (cobra mundo) stdGen
    play (InWindow "hSnake" (tamJanela , tamJanela) (500, 500)) white 60 (mundo { randomGen = gen2, comida = (x,y) }) desenhaMundo tratarEvento atualizaMundo
