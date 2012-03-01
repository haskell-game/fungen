{-
This executable is required for the "make auto" rule, and is a
convenient place for quick tests.
-}

module Main where

import Control.Monad
import Graphics.UI.Fungen
import Text.Printf
-- import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Graphics.UI.GLUT

main :: IO ()
main = do
  funInit
    ((400,400),(250,250),"Hello, Fungen World!") -- window configuration
    (colorMap 0.0 0.0 0.0 250 250)           -- game map
    []                                       -- object managers
    ()                                       -- u
    ()                                       -- t
    [                                        -- input handlers
      (Char 'q',
       Press,
       do liftIOtoIOGame $ putStrLn "q pressed"
      ),
      (MouseButton LeftButton,
       Press,
       do liftIOtoIOGame $ putStrLn "left mouse button pressed"
      )
    ]
    (do                                      -- iogame action
        return ()
    )
    Idle                                     -- refresh type
    []                                       -- file picture list

    -- let winConfig = ((100,20),(width,height),"A brief example!")
    --      bmpList = [("tex.bmp",Nothing)]
    --      gameMap = textureMap 0 30 30 w h
    --      bar    = objectGroup "barGroup"  [createBar]
    --      ball   = objectGroup "ballGroup" [createBall]
    --      initScore = Score 0
    --      input = [(SpecialKey KeyRight, StillDown, moveBarToRight),
    --               (SpecialKey KeyLeft,  StillDown, moveBarToLeft)]
    --  funInit winConfig gameMap [bar,ball] () initScore input gameCycle (Timer 30) bmpList
