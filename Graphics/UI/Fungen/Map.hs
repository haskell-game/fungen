{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This Fungen module contains the map (game background) routines.

-}

module Graphics.UI.Fungen.Map (
        GameMap,
        Tile, TileMatrix,
        getTilePictureIndex, getTileBlocked, getTileMoveCost, getTileSpecialAttribute,
        colorMap, textureMap, tileMap, multiMap,
        getMapSize,
        isTileMap, getTileMapTileMatrix, getTileMapScroll, getTileMapSize, getTileMapTileSize,
        getCurrentMap, updateCurrentMap, updateCurrentIndex, isMultiMap,
        drawGameMap, clearGameScreen
)where

import Graphics.UI.Fungen.Types
import Graphics.UI.Fungen.Util
import Graphics.Rendering.OpenGL

type Tile t = (Int,Bool,Float,t) -- index of picture, possibility to move, cost to move, additional params
type TileMatrix t = [[(Tile t)]]
type TileLine t = [(Tile t)]

data GameMap t
        = ColorMap (Color4 GLclampf) Point2D -- color of the map / size of the map
        | TextureMap Int Point2D Point2D Point2D Point2D  -- texture id / size of texture / present scroll (visible window bottom & left) / scroll speed (X,Y) / size of the map
        | TileMap (TileMatrix t)  Point2D Point2D Point2D Point2D  -- texture handles / tiles matrix / size of tile / present scroll (visible window bottom & left) / scroll speed (X,Y) / size of the map
        | MultiMap [(GameMap t)] Int -- list of maps/current map
--      | PolygMap [Primitive]

getMapSize :: GameMap t -> Point2D
getMapSize (ColorMap _ s) = s
getMapSize (TextureMap _ _ _ _ s) = s
getMapSize (TileMap _ _ _ _ s) = s
getMapSize (MultiMap _ _) = error "Map.getMapSize error: getMapSize cannot be applied with MultiMaps!"

----------------------------
-- special TileMap routines
----------------------------

isTileMap ::  GameMap t -> Bool
isTileMap (TileMap _ _ _ _ _) = True
isTileMap _ = False

getTileMapTileMatrix :: GameMap t -> TileMatrix t
getTileMapTileMatrix (TileMap m _ _ _ _) = m
getTileMapTileMatrix _ = error "Map.getTileMapTileMatrix error: game map is not a tile map!"

getTileMapTileSize :: GameMap t -> Point2D
getTileMapTileSize (TileMap _ ts _ _ _) = ts
getTileMapTileSize _ = error "Map.getTileMapTileSize error: game map is not a tile map!"

getTileMapScroll :: GameMap t -> Point2D
getTileMapScroll (TileMap _ _ s _ _) = s
getTileMapScroll _ = error "Map.getTileMapScroll error: game map is not a tile map!"

getTileMapSize :: GameMap t -> Point2D
getTileMapSize (TileMap _ _ _ _ s) = s
getTileMapSize _ = error "Map.getTileMapSize error: game map is not a tile map!"


------------------------------
-- get routines for a Tile
------------------------------

getTilePictureIndex :: Tile t -> Int
getTilePictureIndex (i,_,_,_) = i

getTileBlocked :: Tile t -> Bool
getTileBlocked (_,b,_,_) = b

getTileMoveCost :: Tile t -> Float
getTileMoveCost (_,_,m,_) = m

getTileSpecialAttribute:: Tile t -> t
getTileSpecialAttribute (_,_,_,t) = t


-------------------------------
-- get routines for a MultiMap
-------------------------------

getCurrentMap :: GameMap t -> GameMap t
getCurrentMap (MultiMap l i) = (l !! i)
getCurrentMap _ = error "Map.getCurrentMap error: getCurrentMap can only be applied with MultiMaps!"

updateCurrentMap :: GameMap t -> GameMap t -> GameMap t
updateCurrentMap (MultiMap l i) newMap = MultiMap (newMapList l newMap i) i
updateCurrentMap _ _ = error "Map.updateCurrentMap error: updateCurrentMap can only be applied with MultiMaps!"

-- internal use only!
newMapList :: [(GameMap t)] -> GameMap t -> Int -> [(GameMap t)]
newMapList [] _ _ = error "Map.newMapList error: please report this bug to awbf@uol.com.br"
newMapList (_:ms) newMap 0 = newMap:ms
newMapList (m:ms) newMap n = m:(newMapList ms newMap (n - 1))

isMultiMap :: GameMap t -> Bool
isMultiMap (MultiMap _ _) = True
isMultiMap _ = False

updateCurrentIndex :: GameMap t -> Int -> GameMap t
updateCurrentIndex (MultiMap mapList _) i | (i >= (length mapList)) = error "Map.updateMultiMapIndex error: map index out of range!"
					  | otherwise = (MultiMap mapList i)
updateCurrentIndex _ _ = error "Map.updateCurrentIndex error: the game map is not a MultiMap!"

-----------------------------
-- creation of maps
-----------------------------

-- creates a PreColorMap
colorMap :: GLclampf -> GLclampf -> GLclampf -> GLdouble -> GLdouble -> GameMap t
colorMap r g b sX sY = ColorMap (Color4 r g b 1.0) (sX,sY)

-- creates a PreTextureMap
textureMap :: Int -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GameMap t
textureMap texId tX tY sX sY = TextureMap texId (tX,tY) (0,0) (0,0) (sX,sY)

-- creates a PreTileMap, cheking if the tileMatrix given is valid and automatically defining the map size
tileMap :: TileMatrix t -> GLdouble -> GLdouble -> GameMap t
tileMap matrix tX tY | matrixOk matrix = TileMap matrix (tX,tY) (0,0) (0,0) (sX,sY)
                     | otherwise = error "Map.tileMap error: each line of your TileMap must have the same number of tiles!"
                   where sX = ((fromIntegral.length.head) matrix) * tX
                         sY = ((fromIntegral.length) matrix) * tY

-- creates a multimap
multiMap :: [(GameMap t)] -> Int -> GameMap t
multiMap [] _ = error "Map.multiMap  error: the MultiMap map list should not be empty!"
multiMap mapList currentMap | (currentMap >= (length mapList)) = error "Map.multiMap error: map index out of range!"
			    | (mapListContainsMultiMap mapList) = error "Map.multiMap error: a MultiMap should not contain another MultiMap!"
			    | otherwise = MultiMap mapList currentMap

-- checks if a GameMap list contains a multimap (internal use only!)
mapListContainsMultiMap :: [(GameMap t)] -> Bool
mapListContainsMultiMap [] = False
mapListContainsMultiMap (a:as) | (isMultiMap a) = True
			       | otherwise = mapListContainsMultiMap as

-- cheks if the tile matrix is a square matrix
matrixOk :: TileMatrix t -> Bool
matrixOk [] = False
matrixOk (m:ms) = matrixOkAux (length m) ms

matrixOkAux :: Int -> TileMatrix t -> Bool
matrixOkAux _ [] = True
matrixOkAux s (m:ms) | (length m) == s = matrixOkAux s ms
                     | otherwise = False


----------------------------------------
--       MAP DRAWING ROUTINES         --
----------------------------------------
clearGameScreen :: GLclampf -> GLclampf -> GLclampf -> IO ()
clearGameScreen r g b = do
        clearColor $= (Color4 r g b 1.0)
        clear [ColorBuffer]

-- draws the background map
drawGameMap :: GameMap t -> Point2D -> [TextureObject] -> IO ()
drawGameMap (ColorMap c _) _ _ = do
        clearColor $= c
        clear [ColorBuffer]
        clearColor $= (Color4 0 0 0 0) -- performance drawback?
drawGameMap (TextureMap texId (tX,tY) (vX,vY) _ _) winSize texList = do
        texture Texture2D $= Enabled
        bindTexture Texture2D (texList !! texId)
        drawTextureMap (tX,tY) (new_winX, new_winY) winSize new_winY texList
        texture Texture2D $= Disabled
        where new_winX | (vX >= 0) = - vX
                       | otherwise = - vX - tX
              new_winY | (vY >= 0) = - vY
                       | otherwise = - vY - tY
drawGameMap (TileMap matrix size visible _ _) winSize texList = do
        texture Texture2D $= Enabled
        drawTileMap (reverse matrix) size visible winSize 0.0 texList
        texture Texture2D $= Disabled
drawGameMap (MultiMap _ _) _ _ = error "Map.drawGameMap error: drawGameMap cannot be applied with MultiMaps!"

-- size of texture, drawing position relative to (X,Y) axis of window, lowest Y drawing position
drawTextureMap :: Point2D -> Point2D -> Point2D -> GLdouble -> [TextureObject] -> IO ()
drawTextureMap (tX,tY) (winX,winY) (winWidth,winHeight) baseY texList
        | (winY > winHeight) = drawTextureMap (tX,tY) (winX + tX, baseY) (winWidth,winHeight) baseY texList
        | (winX > winWidth) = return ()
        | otherwise = do
                loadIdentity
                translate (Vector3 winX winY (0 :: GLdouble) )
                color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
                renderPrimitive Quads $ do
                        texCoord $ TexCoord2 0.0 (0.0 :: GLdouble);  vertex $ Vertex3 0.0 0.0 (0.0 :: GLdouble)
                        texCoord $ TexCoord2 1.0 (0.0 :: GLdouble);  vertex $ Vertex3  tX 0.0 (0.0 :: GLdouble)
                        texCoord $ TexCoord2 1.0 (1.0 :: GLdouble);  vertex $ Vertex3  tX  tY (0.0 :: GLdouble)
                        texCoord $ TexCoord2 0.0 (1.0 :: GLdouble);  vertex $ Vertex3 0.0  tY (0.0 :: GLdouble)
                drawTextureMap (tX,tY) (winX,winY + tY) (winWidth,winHeight) baseY texList
                
-- textures handles, tile matrix, size of texture, (X,Y) scroll, drawing position relative to Y axis of window
drawTileMap :: TileMatrix t -> Point2D -> Point2D -> Point2D -> GLdouble -> [TextureObject] -> IO ()
drawTileMap [] _ _ _ _ _ = return () -- no more tile lines to drawn, so we're done here!
drawTileMap (a:as) (tX,tY) (sX,sY) (winWidth,winHeight) winY texList
        | (sY >= tY) = drawTileMap as (tX,tY) (sX,sY-tY) (winWidth,winHeight) winY texList -- scrolls in the Y axis
        | (winY > winHeight) = return () -- drawing position is higher than the Y window coordinate
        | otherwise = do
                drawTileMapLine a (tX,tY) sX (0.0,winY-sY) winWidth texList
                drawTileMap as (tX,tY) (sX,sY) (winWidth,winHeight) (winY - sY + tY) texList

-- textures handles, tile line, size of texture, X scroll, drawing position relative to (X,Y) axis of window            
drawTileMapLine :: TileLine t -> Point2D -> GLdouble -> Point2D -> GLdouble -> [TextureObject] -> IO ()
drawTileMapLine [] _ _ _ _ _ = return () -- no more tiles to drawn, so we're done here!
drawTileMapLine (a:as) (tX,tY) sX (winX,winY) winWidth texList
        | (sX >= tX) = drawTileMapLine as (tX,tY) (sX-tX) (winX,winY) winWidth texList -- scrolls in the X axis
        | (winX > winWidth) = return () -- drawing position is higher than the X window coordinate
        | otherwise = do
                bindTexture Texture2D (texList !! (getTilePictureIndex a))
                loadIdentity
                translate (Vector3 (new_winX) winY (0 :: GLdouble) )
                color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
                renderPrimitive Quads $ do
                        texCoord $ TexCoord2 0.0 (0.0 :: GLdouble);  vertex $ Vertex3 0.0 0.0 (0.0 :: GLdouble)
                        texCoord $ TexCoord2 1.0 (0.0 :: GLdouble);  vertex $ Vertex3  tX 0.0 (0.0 :: GLdouble)
                        texCoord $ TexCoord2 1.0 (1.0 :: GLdouble);  vertex $ Vertex3  tX  tY (0.0 :: GLdouble)
                        texCoord $ TexCoord2 0.0 (1.0 :: GLdouble);  vertex $ Vertex3 0.0  tY (0.0 :: GLdouble)
                drawTileMapLine as (tX,tY) sX (new_winX + sX + tX,winY) winWidth texList
                where new_winX | (sX >= 0) = winX + sX
                               | otherwise = winX + sX