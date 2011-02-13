{- | 
This Fungen module contains the initialization procedures.
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Init (
        funInit
)where

import Graphics.UI.Fungen.Types
import Graphics.UI.Fungen.Loader(FilePictureList)
import Graphics.UI.Fungen.Display
import Graphics.UI.Fungen.Input
import Graphics.UI.Fungen.Map
import Graphics.UI.Fungen.Objects
import Graphics.UI.Fungen.Game
import Graphics.UI.Fungen.Timer
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

funInit :: WindowConfig -> GameMap v -> [(ObjectManager s)] -> u -> t -> [InputConfig t s u v] -> IOGame t s u v () -> RefreshType -> FilePictureList -> IO ()
funInit winConfig@((px,py),(sx,sy),t) userMap objectGroups gState gAttrib i gameCicle r picList = do
        initialize "FunGen app" []
        createWindow t -- (return ()) [ Double, RGBA ]
        windowPosition $= Position (fromIntegral px) (fromIntegral py)
        windowSize     $= Size     (fromIntegral sx) (fromIntegral sy)
        basicInit sx sy
        game <- createGame userMap objectGroups winConfig gState gAttrib picList
        (bindKey, stillDown) <- funBinding i game
        displayCallback $= (display game gameCicle)
        setRefresh r stillDown
        mainLoop
        
basicInit :: Int -> Int -> IO ()
basicInit sx sy = do
        clearColor $= (Color4 0 0 0 0)
        clear [ColorBuffer]
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        hint PerspectiveCorrection $= Nicest
        matrixMode $= Projection
        loadIdentity
        ortho 0.0 (fromIntegral sx) 0.0 (fromIntegral sy) (-1.0) 1.0
        matrixMode $= Modelview 0
        loadIdentity