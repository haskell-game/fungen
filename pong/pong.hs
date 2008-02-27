{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This is a very simple FunGEn example.
In order to use the Makefile, set the HC, FG (FunGEn dir) and TOP variables correctly.

-}

module Main where

import FunGEn

data GameAttribute = Score Int
 
type PongObject = GameObject ()
type PongAction a = IOGame GameAttribute () () () a

main :: IO ()
main = do
        let winConfig = ((0,0),(250,250),"A brief example!")
	    bmpList = [("tex.bmp",Nothing)]
	    gameMap = textureMap 0 50 50 250.0 250.0
            bar    = objectGroup "barGroup"  [createBar]
            ball   = objectGroup "ballGroup" [createBall]
            initScore = Score 0
            input = [(SpecialKey KeyRight, StillDown, moveBarToRight),
                     (SpecialKey KeyLeft,  StillDown, moveBarToLeft)]

        funInit winConfig gameMap [bar,ball] () initScore input gameCycle (Timer 40) bmpList

createBall :: PongObject
createBall = let ballPic = Basic (Circle 3.0 0.0 1.0 0.0 Filled)
	     in object "ball" ballPic False (125,125) (-5,5) ()


createBar :: PongObject
createBar = let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
                barPic = Basic (Polyg barBound 1.0 1.0 1.0 Unfilled)
            in object "bar" barPic False (125,30) (0,0) ()

moveBarToRight :: PongAction ()
moveBarToRight = do
        obj <- findObject "bar" "barGroup"
        (pX,pY) <- getObjectPosition obj
        (sX,_)  <- getObjectSize obj
        if (pX + (sX/2) + 5 <= 250)
        	then (setObjectPosition ((pX + 5),pY) obj)
        	else (setObjectPosition ((250 - (sX/2)),pY) obj)

moveBarToLeft :: PongAction ()
moveBarToLeft = do
        obj <- findObject "bar" "barGroup"
        (pX,pY) <- getObjectPosition obj
        (sX,_)  <- getObjectSize obj
        if (pX - (sX/2) - 5 >= 0)
        	then (setObjectPosition ((pX - 5),pY) obj)
        	else (setObjectPosition (sX/2,pY) obj)

gameCycle :: PongAction ()
gameCycle = do
        (Score n) <- getGameAttribute
        printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0

	ball <- findObject "ball" "ballGroup"
        col1 <- objectLeftMapCollision ball
        col2 <- objectRightMapCollision ball
        when (col1 || col2) (reverseXSpeed ball)
        col3 <- objectTopMapCollision ball
        when col3 (reverseYSpeed ball)
        col4 <- objectBottomMapCollision ball
        when col4 (funExit)

        bar <- findObject "bar" "barGroup"
        col5 <- objectsCollision ball bar
        when col5
        	(do reverseYSpeed ball
        	    setGameAttribute (Score (n + 10)))
	showFPS TimesRoman24 (30,0) 1.0 0.0 0.0



