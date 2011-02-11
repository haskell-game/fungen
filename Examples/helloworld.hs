module Main where

import Graphics.UI.FunGEn

main :: IO ()
main = let  winConfig = ((0,0),(250,250),"Hello, FunGEn World!")
	    gameMap = colorMap 0.0 0.0 0.0 250 250
       in funInit winConfig gameMap [] () () [] (return()) Idle []