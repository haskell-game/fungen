{-# OPTIONS_HADDOCK hide #-}
{- | 
This FunGEn module controls timing (how time-based functions will behave).
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Timer (
        RefreshType(..),
        setRefresh
) where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Input

-- | Used by 'Graphics.UI.Fungen.funInit' to configure the main loop's timing strategy.
data RefreshType
        = Idle
        | Timer Int

-- | Change the current timing strategy.
setRefresh :: RefreshType -> StillDownHandler -> IO ()
setRefresh Idle stillDown = idleCallback $= Just (stillDown >> postRedisplay Nothing)
setRefresh (Timer t) stillDown = addTimerCallback t (timer stillDown t)

-- | Generate a GLUT timer callback.
timer :: StillDownHandler -> Int -> TimerCallback
timer stillDown t = do
        stillDown
        postRedisplay Nothing
        addTimerCallback t (timer stillDown t)