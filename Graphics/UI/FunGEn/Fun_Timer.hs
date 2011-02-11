{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module controls timing (how time-based functions will behave).

-}

module Graphics.UI.FunGEn.Fun_Timer (
        RefreshType(..),
        setRefresh
) where

import Graphics.UI.FunGEn.UserInput
import Graphics.UI.GLUT

data RefreshType
        = Idle
        | Timer Int

setRefresh :: RefreshType -> StillDownHandler -> IO ()
setRefresh Idle stillDown = idleCallback $= Just (stillDown >> postRedisplay Nothing)
setRefresh (Timer t) stillDown = addTimerCallback t (timer stillDown t)

timer :: StillDownHandler -> Int -> TimerCallback
timer stillDown t = do
        stillDown
        postRedisplay Nothing
        addTimerCallback t (timer stillDown t)