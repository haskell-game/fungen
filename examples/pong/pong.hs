{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This is a very simple Fungen example.
In order to use the Makefile, set the HC, FG (Fungen dir) and TOP variables correctly.

-}

module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int
 
type PongObject = GameObject ()
type PongAction a = IOGame GameAttribute () () () a

width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
        let winConfig = ((100,20),(width,height),"A brief example!")
	    bmpList = [("tex.bmp",Nothing)]
	    gameMap = textureMap 0 30 30 w h
            bar    = objectGroup "barGroup"  [createBar]
            ball   = objectGroup "ballGroup" [createBall]
            initScore = Score 0
            input = [(SpecialKey KeyRight, StillDown, moveBarToRight),
                     (SpecialKey KeyLeft,  StillDown, moveBarToLeft)]

        funInit winConfig gameMap [bar,ball] () initScore input gameCycle (Timer 30) bmpList

createBall :: PongObject
createBall = let ballPic = Basic (Circle 6.0 0.0 1.0 0.0 Filled)
	     in object "ball" ballPic False (w/2,h/2) (-8,8) ()


createBar :: PongObject
createBar = let barBound = [(-25,-6),(25,-6),(25,6),(-25,6)]
                barPic = Basic (Polyg barBound 1.0 1.0 1.0 Filled)
            in object "bar" barPic False (w/2,30) (0,0) ()

moveBarToRight :: PongAction ()
moveBarToRight = do
        obj <- findObject "bar" "barGroup"
        (pX,pY) <- getObjectPosition obj
        (sX,_)  <- getObjectSize obj
        if (pX + (sX/2) + 5 <= w)
        	then (setObjectPosition ((pX + 5),pY) obj)
        	else (setObjectPosition ((w - (sX/2)),pY) obj)

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
        when col4 (do reverseYSpeed ball) -- (funExit)

        bar <- findObject "bar" "barGroup"
        col5 <- objectsCollision ball bar
        let (_,vy) = getGameObjectSpeed ball
        when (and [col5, vy < 0])  (do reverseYSpeed ball
        	                       setGameAttribute (Score (n + 10)))
	showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0

