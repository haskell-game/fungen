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
        Key(..), KeyEvent(..), SpecialKey(..), MouseButton(..), Modifiers(..), Position(..),
        funBinding
) where

import Graphics.UI.Fungen.Game
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Input (KeyEvent(..), KeyBinder, StillDownHandler, initGLUTInput)

-- | A FunGEn input handler (which we use instead of GLUTInput's) is
-- an IOGame (game action) that takes two extra arguments: the current
-- keyboard modifiers state, and the current mouse position. (For a StillDown
-- event, these will be the original state and position from the Press event.)
type InputHandler t s u v = Modifiers -> Position -> IOGame t s u v ()

-- | A mapping from an input event to an input handler.
type InputBinding t s u v = (Key, KeyEvent, InputHandler t s u v)

funBinding :: [InputBinding t s u v] -> Game t s u v -> IO (KeyBinder, StillDownHandler)
funBinding inputs g = do
        (bindKey, stillDown) <- initGLUTInput
        mapM (userBinding g bindKey) inputs 
        return (bindKey, stillDown)

userBinding :: Game t s u v -> KeyBinder -> InputBinding t s u v -> IO ()
userBinding g bindKey (key, keyEvent, inputHandler) = bindKey key keyEvent (Just (\mods pos -> runIOGameM (inputHandler mods pos) g))