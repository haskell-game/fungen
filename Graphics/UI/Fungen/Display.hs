{- | This Fungen module renders the game window.
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

| This Fungen module renders the game window.

-}

module Graphics.UI.Fungen.Display (
        display
) where

import Graphics.UI.Fungen.Game
import Graphics.UI.Fungen.Util (when)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- | Given a fungen Game and IOGame, generate an opengl display handler.
display :: Game t s u v -> IOGame t s u v () -> DisplayCallback
display g gameCycle = do 
        clear [ColorBuffer]
        runIOGame (displayIOGame gameCycle) g
        swapBuffers
        flush

-- | Run one update and display an IOGame.
displayIOGame :: IOGame t s u v () -> IOGame t s u v ()
displayIOGame gameCycle = do
        (_,_,objectsMoving) <- getGameFlags
        when objectsMoving moveAllObjects
	gameCycle
        (mapDrawing,objectsDrawing,_) <- getGameFlags
        when mapDrawing drawMap
        when objectsDrawing drawAllObjects
        printText    
