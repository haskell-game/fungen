module Main where

import Graphics.UI.Fungen

main :: IO ()
main = let  winConfig = ((0,0),(250,250),"Hello, Fungen World!")
	    gameMap = colorMap 0.0 0.0 0.0 250 250
       in funInit winConfig gameMap [] () () [] (return()) Idle []