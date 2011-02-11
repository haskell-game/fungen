{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module contains the FunGEN basic types.

-}

module Graphics.UI.FunGEn.Fun_Types (
        WindowConfig,
        Point2D,
        ColorList3, AwbfBitmap, InvList,
) where

import Graphics.Rendering.OpenGL

type WindowConfig = ((Int,Int),(Int,Int),String)        -- position, size and name of the window
type Point2D = (GLdouble,GLdouble)                      -- a bidimensional point in space
type ColorList3 = [(GLubyte, GLubyte, GLubyte)]         -- color in RGB format
type AwbfBitmap = (GLsizei, GLsizei, PixelData GLubyte)   -- width, height and data of bitmap
type InvList = Maybe [(Int,Int,Int)]                    -- invisible colors (in RGB) of bitmap
