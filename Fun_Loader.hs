{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module loads [bmp] files.

-}

module Fun_Loader (
        loadBitmap, loadBitmapList, FilePictureList
)where

import Graphics.Rendering.OpenGL
import IO
import Foreign
import Fun_Types
import Fun_Aux
import Fun_Types(ColorList3, AwbfBitmap)

binAux :: String
binAux = "000000000000000000000000"

type BmpList = [(GLubyte, GLubyte, GLubyte, GLubyte)]
type FilePictureList = [(FilePath,InvList)]

--loads a bitmap from a file
loadBitmap :: FilePath -> Maybe ColorList3 -> IO AwbfBitmap
loadBitmap bmName invList = do
        bmFile <- openFile bmName (ReadMode)
        bmString <- hGetContents bmFile
        (bmW,bmH) <- getWH (dropGLsizei 18 bmString)
        bmData <- getBmData (dropGLsizei 54 bmString) (bmW,bmH) invList
        hClose bmFile
        return (bmW,bmH,bmData)

 -- loads n bitmaps from n files        
loadBitmapList :: [(FilePath, Maybe ColorList3)] -> IO [AwbfBitmap]
loadBitmapList bmps = do
        bmList <- loadBmListAux bmps []
        return (reverse bmList)

loadBmListAux :: [(FilePath, Maybe ColorList3)] -> [AwbfBitmap] -> IO [AwbfBitmap]
loadBmListAux [] bmList = return (bmList)
loadBmListAux ((n,l):as) bmList = do
        bm <- loadBitmap n l
        loadBmListAux as (bm:bmList)

getWH :: String -> IO (GLsizei,GLsizei)
getWH (a:b:c:d:e:f:g:h:_) = do
        return ( (op (bin a) 0) + (op (bin b) 8) + (op (bin c) 16) + (op (bin d) 24),
                 (op (bin e) 0) + (op (bin f) 8) + (op (bin g) 16) + (op (bin h) 24))
                 where bin x = toBinary(fromEnum x)
                       op x n = toDecimal(shiftLeft(binAux ++ (make0 (8 - (length x)) ++ x)) n)
getWH _ = error "Fun_Loader.getWH error: strange bitmap file"                    

getBmData :: String -> (GLsizei,GLsizei) -> Maybe ColorList3 -> IO (PixelData GLubyte)
getBmData bmString (bmW,bmH) invList = 
        let colorList = makeColorList bmString (bmW,bmH) in
        newArray [Color4 r g b a | (r,g,b,a) <- addInvisiblity colorList invList] >>= \bmData ->
        return (PixelData RGBA UnsignedByte (castPtr bmData))
        
addInvisiblity :: ColorList3 -> Maybe ColorList3 -> BmpList
addInvisiblity [] _ = []
addInvisiblity l Nothing = map (\(r,g,b) -> (r,g,b,255)) l
addInvisiblity ((r,g,b):as) i@(Just invList) | (r,g,b) `elem` invList = ((r,g,b,0):(addInvisiblity as i))
                                             | otherwise = ((r,g,b,255):(addInvisiblity as i))
                                             
makeColorList :: String -> (GLsizei,GLsizei) -> [(GLubyte, GLubyte, GLubyte)]
makeColorList bmString (bmW,bmH) = makeColorListAux (bmW `mod` 4) bmString (bmW*bmH) (bmW,bmW)
                        
makeColorListAux :: GLsizei -> String -> GLsizei -> (GLsizei,GLsizei) -> [(GLubyte, GLubyte, GLubyte)]
makeColorListAux _ _ 0 _ = []
makeColorListAux x bmString totVert (0,bmW) = makeColorListAux x (dropGLsizei x bmString) totVert (bmW,bmW)
makeColorListAux x (b:g:r:bmString) totVert (n,bmW) = (ord2 r,ord2 g,ord2 b): (makeColorListAux x bmString (totVert - 1) (n - 1,bmW))
makeColorListAux _ _ _ _ = error "Fun_Loader.makeColorListAux error: strange bitmap file"