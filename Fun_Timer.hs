{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module controls timing (how time-based functions will behave).

-}

module Fun_Timer (
        RefreshType(..),
        setRefresh
) where

import UserInput
import GLUT

data RefreshType
        = Idle
        | Timer Int

setRefresh :: RefreshType -> StillDownHandler -> IO ()
setRefresh Idle stillDown = idleFunc (Just (stillDown >> postRedisplay))
setRefresh (Timer t) stillDown = timerFunc t (timer stillDown t)

timer :: StillDownHandler -> Int -> TimerAction
timer stillDown t = do
        stillDown
        postRedisplay
        timerFunc t (timer stillDown t)