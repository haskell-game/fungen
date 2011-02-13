{- | This FunGEn module contains the FunGEN basic types. 
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Types (
        WindowConfig,
        Point2D,
        ColorList3,
        AwbfBitmap,
        InvList,
) where

import Graphics.Rendering.OpenGL

-- | position, size and name of the window
type WindowConfig = ((Int,Int),(Int,Int),String)

-- | a bidimensional point in space
type Point2D = (GLdouble,GLdouble)

-- | color in RGB format
type ColorList3 = [(GLubyte, GLubyte, GLubyte)]

-- | width, height and data of bitmap
type AwbfBitmap = (GLsizei, GLsizei, PixelData GLubyte)

-- | invisible colors (in RGB) of bitmap
type InvList = Maybe [(Int,Int,Int)]



