{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module contains some auxiliary functions.

-}

module Fun_Aux (
        texCoord2, vertex3, texStuff,
        toRad,
        randInt, randFloat, randDouble,
        shiftLeft, toDecimal, pow2, toBinary, make0, dropGLsizei,
        ord2,
        addNoInvisibility,
        racMod,
        matrixToList, matrixSize, position,
        inv2color3, pathAndInv2color3List, point2DtoVertex3,
        isEmpty,
        when, unless
) where

import Fun_Types
import GL
import Random

texCoord2 :: GLdouble -> GLdouble -> IO ()
texCoord2 x y = texCoord (TexCoord2 x y)

vertex3 :: GLdouble -> GLdouble -> GLdouble -> IO ()
vertex3 x y z = vertex (Vertex3 x y z)

texStuff :: [TextureName] -> [AwbfBitmap] -> IO ()
texStuff [] _ = return ()
texStuff (t:ts) ((bmW,bmH,bmData):bms) = do
        bindTexture Texture2d t
        texImage2D Texture2d 0 GL.Rgba' bmW bmH 0 bmData
        texParameter Texture2d (TextureFilters Nearest Nearest)
        texStuff ts bms
texStuff _ _ = return ()

toRad :: Float -> Float
toRad a = ((pi * a)/180)

randInt :: (Int,Int) -> IO Int
randInt (a,b) = randomRIO (a,b)

randFloat :: (Float,Float) -> IO Float
randFloat (a,b) = randomRIO (a,b)

randDouble :: (Double,Double) -> IO Double
randDouble (a,b) = randomRIO (a,b)

shiftLeft :: String -> Int -> String
shiftLeft a 0 = a
shiftLeft (_:as) n = shiftLeft(as ++ "0") (n-1)
shiftLeft _ _ = []

toDecimal :: String -> GLsizei
toDecimal a = toDecimalAux (reverse a) 32

toDecimalAux :: String -> GLsizei -> GLsizei
toDecimalAux [] _ = 0
toDecimalAux _ 0 = 0
toDecimalAux (a:as) n
                | a == '0' = toDecimalAux as (n-1)
                | otherwise = pow2 (32 - n) + toDecimalAux as (n-1)
                
pow2 :: GLsizei -> GLsizei
pow2 0 = 1
pow2 n = 2 * pow2(n-1)

toBinary :: Int -> String
toBinary n
        | n < 2 = show n
        | otherwise = toBinary (n `div` 2) ++ (show (n `mod` 2))
        
make0 :: Int -> String
make0 0 = []
make0 n = '0':(make0 (n-1))

ord2 :: Char -> GLubyte
ord2 a = (toEnum.fromEnum) a

dropGLsizei                :: GLsizei -> [a] -> [a]
dropGLsizei 0 xs            = xs
dropGLsizei _ []            = []
dropGLsizei n (_:xs) | n>0  = dropGLsizei (n-1) xs
dropGLsizei _ _ = error "Fun_Aux.dropGLsizei error: negative argument"

-- to be used when no invisibility must be added when loading a file
addNoInvisibility :: [FilePath] -> [(FilePath, Maybe ColorList3)]
addNoInvisibility [] = []
addNoInvisibility (a:as) = (a, Nothing):(addNoInvisibility as)

racMod :: GLdouble -> GLdouble -> GLdouble
racMod a b | (a >= 0) = racModPos a b
           | otherwise = racModNeg a b

racModPos :: GLdouble -> GLdouble -> GLdouble
racModPos a b | (a - b < 0) = a
              | otherwise = racModPos (a - b) b

racModNeg :: GLdouble -> GLdouble -> GLdouble
racModNeg a b | (a + b > 0) = a
              | otherwise = racModPos (a + b) b

matrixToList :: [[a]] -> [a]
matrixToList [] = []
matrixToList (a:as) = a ++ (matrixToList as)

-- return the max indexes of a matrix (assumed that its lines have the same length)
matrixSize ::  [[a]] -> (Int,Int)
matrixSize [] = (0,0)
matrixSize m@(a:_) = ((length m) - 1,(length a) - 1)

-- returns the position of an element in a list (the first elem has position zero)
position :: (Eq t, Show t) => t -> [t] -> Int
position x (a:as) | x == a = 0
                  | otherwise = 1 + position x as
position x [] = error ("Fun_Aux.position: " ++ (show x) ++ " is not an element of list.")

inv2color3 :: InvList -> Maybe ColorList3
inv2color3 Nothing = Nothing
inv2color3 (Just l) = Just (inv2color3Aux l)

inv2color3Aux :: [(Int,Int,Int)] -> [(GLubyte,GLubyte,GLubyte)]
inv2color3Aux [] = []
inv2color3Aux ((r,g,b):ls) = (z r,z g,z b):(inv2color3Aux ls)
                 where z = toEnum.fromEnum

pathAndInv2color3List :: (FilePath,InvList) -> (FilePath, Maybe ColorList3)
pathAndInv2color3List (f,Nothing) = (f,Nothing)
pathAndInv2color3List (f,Just l) = (f,Just (inv2color3Aux l))

point2DtoVertex3 :: [Point2D] -> [Vertex3 GLdouble]
point2DtoVertex3 [] = []
point2DtoVertex3 ((x,y):as) = (Vertex3 x y 0.0):(point2DtoVertex3 as)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

when         :: (Monad m) => Bool -> m () -> m ()
when p s      = if p then s else return ()

unless       :: (Monad m) => Bool -> m () -> m ()
unless p s    = when (not p) s