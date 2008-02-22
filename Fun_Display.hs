{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module renders the game window.

-}

module Fun_Display (
        display
) where

import Fun_Game
import Fun_Aux (when)
import GL
import GLUT

display :: Game t s u v -> IOGame t s u v () -> DisplayAction
display g gameCycle = do 
        clear [ColorBufferBit]
        runIOGame (displayIOGame gameCycle) g
        swapBuffers
        flush

displayIOGame :: IOGame t s u v () -> IOGame t s u v ()
displayIOGame gameCycle = do
        (_,_,objectsMoving) <- getGameFlags
        when objectsMoving moveAllObjects
	gameCycle
        (mapDrawing,objectsDrawing,_) <- getGameFlags
        when mapDrawing drawMap
        when objectsDrawing drawAllObjects
        printText    
