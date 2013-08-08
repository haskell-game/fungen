{-# OPTIONS_HADDOCK hide #-}
{- | 
This FunGEn module controls the user input (mouse, keyboard, joystick...)
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Input (
        InputBinding, InputHandler,
        KeyEvent(..), Key(..), SpecialKey(..), MouseButton(..), Modifiers(..), Position(..),
        funInitInput
) where

import Graphics.UI.Fungen.Game
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Input (KeyEvent(..), KeyBinder, StillDownHandler, glutInitInput)

-- | A FunGEn input handler is like an IOGame (game action) that takes
-- two extra arguments: the current keyboard modifiers state, and the
-- current mouse position. (For a StillDown event, these will be the
-- original state and position from the Press event.)
type InputHandler t s u v = Modifiers -> Position -> IOGame t s u v ()

-- | A mapping from an input event to an input handler.
type InputBinding t s u v = (Key, KeyEvent, InputHandler t s u v)

-- | Initialise the input system, which keeps a list of input event to
-- action bindings and executes the the proper actions automatically.
-- Returns a function for adding bindings (GLUT's - should return the
-- FunGEn-aware one instead ?), and another which should be called
-- periodically (eg from refresh) to trigger still-down actions.
funInitInput :: [InputBinding t s u v] -> Game t s u v -> IO (KeyBinder, StillDownHandler)
funInitInput bindings game = do
  (glutBindKey, glutStillDown) <- glutInitInput
  let funBindKey (key, keyEvent, inputHandler) =
        glutBindKey key keyEvent (Just (\mods pos -> runIOGameM (inputHandler mods pos) game))
  mapM funBindKey bindings
  return (glutBindKey, glutStillDown)
