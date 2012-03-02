{-
This executable is used by the "make auto" rule, and is a good place for experiments.
It probably needs to be compiled to work.
-}

module Main where

import Control.Monad
import Graphics.UI.Fungen
import Text.Printf
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
      -- q, shift-q, control-q etc. must be matched as different characters, at least on the mac:
      (Char 'q', Press,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "q pressed with mods %s, pos %s" (show mods) (show pos)
      ),
      (Char 'Q', Press,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "shift q pressed with mods %s, pos %s" (show mods) (show pos)
      ),
      (Char '\DC1', Press,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "control q pressed with mods %s, pos %s" (show mods) (show pos)
      ),
      (MouseButton LeftButton, Press,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "left mouse button pressed with mods %s, pos %s" (show mods) (show pos)
      ),
      -- still down handler - mods and pos remain at the initial values
      (MouseButton LeftButton, StillDown,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "left mouse button still down with mods %s, pos %s" (show mods) (show pos)
      ),
      (MouseButton LeftButton, Release,
       \mods pos -> do liftIOtoIOGame $ putStrLn $ printf "left mouse button released with mods %s, pos %s" (show mods) (show pos)
      )
    ]
    (do                                      -- iogame action
        return ()
    )
    Idle                                     -- refresh type
    []                                       -- file picture list
