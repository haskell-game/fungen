{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module contains the initialization procedures.

-}

module Fun_Init (
        funInit,
        WindowConfig
)where

import Fun_Types
import Fun_Loader(FilePictureList)
import Fun_Display
import Fun_Input
import Fun_Map
import Fun_Objects
import Fun_Game
import Fun_Timer
import GL
import GLUT

funInit :: WindowConfig -> GameMap v -> [(ObjectManager s)] -> u -> t -> [InputConfig t s u v] -> IOGame t s u v () -> RefreshType -> FilePictureList -> IO ()
funInit winConfig@((px,py),(sx,sy),t) userMap objectGroups gState gAttrib i gameCicle r picList = do
        GLUT.init Nothing
        createWindow t (return ()) [ GLUT.Double, GLUT.Rgba ]
                (Just (WindowPosition px py))
                (Just (WindowSize     sx sy))
        basicInit sx sy
        game <- createGame userMap objectGroups winConfig gState gAttrib picList
        (bindKey, stillDown) <- funBinding i game
        displayFunc (display game gameCicle)
        setRefresh r stillDown
        mainLoop
        
basicInit :: Int -> Int -> IO ()
basicInit sx sy = do
        clearColor (Color4 0 0 0 0)
        clear [ColorBufferBit]
        enable Blend'
        blendFunc SrcAlpha OneMinusSrcAlpha
        hint PerspectiveCorrectionHint Nicest
        matrixMode Projection
        loadIdentity
        ortho 0.0 (fromIntegral sx) 0.0 (fromIntegral sy) (-1.0) 1.0
        matrixMode Modelview
        loadIdentity