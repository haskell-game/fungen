{-# OPTIONS_HADDOCK hide #-}
{- | 
This FunGEn module contains some functions to print text on the screen.
Fonts supported: Bitmap9By15, Bitmap8By13, BitmapTimesRoman10, BitmapTimesRoman24
		 BitmapHelvetica10, BitmapHelvetica12, BitmapHelvetica18
-}
{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Text (
	BitmapFont(..),
	Text,
	putGameText
) where

import Graphics.UI.GLUT
import Graphics.UI.Fungen.Types
import Graphics.Rendering.OpenGL

type Text = (String,BitmapFont,Point2D,GLclampf,GLclampf,GLclampf)

-- string to be printed, type of font, screen position, color RGB
putGameText :: [Text] -> IO ()
putGameText [] = return ()
putGameText ((text,font,(x,y),r,g,b):ts) = do
	loadIdentity
	color (Color3 r g b)
	rasterPos (Vertex2 x y)
	renderString font text
	putGameText ts