{- | 
This module contains the Fungen objects procedures
-}
{- 
FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Objects (
    GameObject,
    getGameObjectId, getGameObjectName, getGameObjectManagerName, getGameObjectAsleep, getGameObjectPosition, getGameObjectSize, getGameObjectSpeed, getGameObjectAttribute,
    getObjectManagerName, getObjectManagerCounter , getObjectManagerObjects,
    ObjectPicture(..), Primitive(..),
    FillMode (..),
    object, drawGameObjects, drawGameObject,
    objectGroup, ObjectManager,
    findObjectFromId, searchObjectManager, searchGameObject,
    updateObject, updateObjectAsleep, updateObjectSize, updateObjectPosition,
    updateObjectSpeed, updateObjectAttribute, updateObjectPicture,
    addObjectsToManager,
    moveGameObjects,
    destroyGameObject
) where

import Graphics.UI.Fungen.Types
import Graphics.UI.Fungen.Util
import Graphics.Rendering.OpenGL hiding (Primitive)
import Graphics.UI.GLUT hiding (Primitive)

data GameObject t = GO {
    objId	   :: Integer,
    objName        :: String,
    objManagerName :: String,
    objPicture     :: GameObjectPicture,
    objAsleep      :: Bool,
    objSize        :: Point2D,
    objPosition    :: Point2D,
    objSpeed       :: Point2D,
    objAttribute   :: t
    }

data ObjectManager t = OM {
    mngName    :: String,		-- name of the manager
    mngCounter :: Integer,		-- next current avaible index for a new object
    mngObjects :: [(GameObject t)]	-- the SET of objects
    }

data GamePrimitive
    = P [Vertex3 GLdouble] (Color4 GLfloat) FillMode
    | C GLdouble (Color4 GLfloat) FillMode

data GameObjectPicture
    = Tx Int
    | B GamePrimitive
--  | A [TextureObject] Int -- miliseconds between frames

data Primitive
    = Polyg [Point2D] GLfloat GLfloat GLfloat FillMode -- the points (must be in CCW order!), color, fill mode
    | Circle GLdouble GLfloat GLfloat GLfloat FillMode -- color, radius, fill mode
    
data ObjectPicture
    = Tex (GLdouble,GLdouble) Int -- size, current texture
    | Basic Primitive
--  | Animation [(FilePath,InvList)] Int -- (path to file, invisible colors), miliseconds between frames

data FillMode
    = Filled
    | Unfilled
    deriving Eq

-------------------------------------------
-- get & updating routines for GameObjects
-------------------------------------------
getGameObjectId :: GameObject t -> Integer
getGameObjectId = objId

getGameObjectName :: GameObject t -> String
getGameObjectName = objName

getGameObjectManagerName :: GameObject t -> String
getGameObjectManagerName = objManagerName

-- internal use only!
getGameObjectPicture :: GameObject t -> GameObjectPicture
getGameObjectPicture = objPicture

getGameObjectAsleep :: GameObject t -> Bool
getGameObjectAsleep = objAsleep

getGameObjectSize :: GameObject t -> (GLdouble,GLdouble)
getGameObjectSize o = (realToFrac sX,realToFrac sY)
                      where (sX,sY) = objSize o

getGameObjectPosition :: GameObject t -> (GLdouble,GLdouble)
getGameObjectPosition o = (realToFrac pX,realToFrac pY)
                          where (pX,pY) = objPosition o

getGameObjectSpeed :: GameObject t -> (GLdouble,GLdouble)
getGameObjectSpeed o = (realToFrac sX,realToFrac sY)
                       where (sX,sY) = objSpeed o

getGameObjectAttribute :: GameObject t -> t
getGameObjectAttribute = objAttribute

updateObjectPicture :: Int -> Int -> GameObject t -> GameObject t
updateObjectPicture newIndex maxIndex obj =
    case (getGameObjectPicture obj) of
        Tx _ -> if (newIndex <= maxIndex)
                        then (obj {objPicture = Tx newIndex})
                        else (error ("Objects.updateObjectPicture error: picture index out of range for object " ++
                              (getGameObjectName obj) ++ " of group " ++ (getGameObjectManagerName obj)))
        _ -> error ("Objects.updateObjectPicture error: object " ++ (getGameObjectName obj) ++
                     " of group " ++ (getGameObjectManagerName obj) ++ " is not a textured object!")

updateObjectAsleep :: Bool -> GameObject t -> GameObject t
updateObjectAsleep asleep o = o {objAsleep = asleep}

updateObjectSize :: (GLdouble,GLdouble) -> GameObject t -> GameObject t
updateObjectSize (sX,sY) o = o {objSize = (realToFrac sX, realToFrac sY)}

updateObjectPosition :: (GLdouble,GLdouble) -> GameObject t -> GameObject t
updateObjectPosition (pX,pY) o = o {objPosition = (realToFrac pX, realToFrac pY)}

updateObjectSpeed :: (GLdouble,GLdouble) -> GameObject t -> GameObject t
updateObjectSpeed (sX,sY) o = o {objSpeed = (realToFrac sX, realToFrac sY)}

updateObjectAttribute :: t -> GameObject t -> GameObject t
updateObjectAttribute oAttrib o = o {objAttribute = oAttrib}

----------------------------------------------
-- get & updating routines for ObjectManagers
----------------------------------------------
getObjectManagerName :: ObjectManager t -> String
getObjectManagerName = mngName

getObjectManagerCounter :: ObjectManager t -> Integer
getObjectManagerCounter = mngCounter

getObjectManagerObjects :: ObjectManager t -> [(GameObject t)]
getObjectManagerObjects = mngObjects

updateObjectManagerObjects :: [(GameObject t)] -> ObjectManager t -> ObjectManager t
updateObjectManagerObjects objs mng = mng {mngObjects = objs}

----------------------------------------
-- initialization of GameObjects
----------------------------------------
object :: String -> ObjectPicture -> Bool -> (GLdouble,GLdouble) -> (GLdouble,GLdouble) -> t -> GameObject t
object name pic asleep pos speed oAttrib = let (picture, size) = createPicture pic in
			GO {
			objId          = 0,
			objName        = name,
			objManagerName = "object not grouped yet!",
			objPicture     = picture,
			objAsleep      = asleep,
			objSize        = size,
			objPosition    = pos,
			objSpeed       = speed,
			objAttribute   = oAttrib
			}

createPicture :: ObjectPicture -> (GameObjectPicture,Point2D)
createPicture (Basic (Polyg points r g b fillMode))  = (B (P (point2DtoVertex3 points) (Color4 r g b 1.0) fillMode),findSize points)
createPicture (Basic (Circle radius r g b fillMode)) = (B (C radius (Color4 r g b 1.0) fillMode),(2 * radius,2 * radius))
createPicture (Tex size picIndex) = (Tx picIndex,size)

-- given a point list, finds the height and width
findSize :: [Point2D] -> Point2D
findSize l = ((x2 - x1),(y2 - y1))
    where (xList,yList) = unzip l
          (x2,y2) = (maximum xList,maximum yList)
          (x1,y1) = (minimum xList,minimum yList)

----------------------------------------------
-- grouping GameObjects and creating managers
----------------------------------------------
objectGroup :: String -> [(GameObject t)] -> (ObjectManager t)
objectGroup name objs = OM {mngName = name, mngCounter = toEnum (length objs), mngObjects = objectGroupAux objs name 0}

objectGroupAux :: [(GameObject t)] -> String -> Integer -> [(GameObject t)]
objectGroupAux [] _ _ = []
objectGroupAux (o:os) managerName oId = (o {objId = oId, objManagerName = managerName}):(objectGroupAux os managerName (oId + 1))

addObjectsToManager :: [(GameObject t)] -> String -> [(ObjectManager t)] -> [(ObjectManager t)]
addObjectsToManager _ managerName [] = error ("Objects.addObjectsToManager error: object manager " ++ managerName ++ " does not exists!")
addObjectsToManager objs managerName (m:ms) | (getObjectManagerName m == managerName) = (addObjectsToManagerAux objs m):ms
					    | otherwise = m:(addObjectsToManager objs managerName ms)


addObjectsToManagerAux :: [(GameObject t)] -> ObjectManager t -> ObjectManager t
addObjectsToManagerAux objs mng = let counter = getObjectManagerCounter mng
				      newObjects = adjustNewObjects objs (getObjectManagerCounter mng) (getObjectManagerName mng)
				  in mng {mngObjects = newObjects ++ (getObjectManagerObjects mng), mngCounter = counter + (toEnum (length objs))}

adjustNewObjects :: [(GameObject t)] -> Integer -> String -> [(GameObject t)]
adjustNewObjects [] _ _ = []
adjustNewObjects (o:os) oId managerName = (o {objId = oId, objManagerName = managerName}):(adjustNewObjects os (oId + 1) managerName)

------------------------------------------
-- draw routines
------------------------------------------
drawGameObjects :: [(ObjectManager t)] -> QuadricPrimitive -> [TextureObject] -> IO ()
drawGameObjects [] _ _ = return ()
drawGameObjects (m:ms) qobj picList = drawGameObjectList (getObjectManagerObjects m) qobj picList >> drawGameObjects ms qobj picList

drawGameObjectList :: [(GameObject t)] -> QuadricPrimitive -> [TextureObject] -> IO ()
drawGameObjectList [] _ _ = return ()
drawGameObjectList (o:os) qobj picList | (getGameObjectAsleep o) = drawGameObjectList os qobj picList
                                       | otherwise = drawGameObject o qobj picList >> drawGameObjectList os qobj picList

drawGameObject :: GameObject t -> QuadricPrimitive -> [TextureObject] -> IO ()
drawGameObject o qobj picList = do
    loadIdentity
    let (pX,pY) = getGameObjectPosition o
        picture = getGameObjectPicture o
    translate (Vector3 pX pY (0 :: GLdouble) )
    case picture of
        (B (P points c fillMode)) -> do
                    color c
                    if (fillMode == Filled)
                        then (renderPrimitive Polygon  $ mapM_ vertex points)
                        else (renderPrimitive LineLoop $ mapM_ vertex points) 

        (B (C r c fillMode)) -> do
                    color c
                    renderQuadric style $ Disk 0 r 20 3
                 where style = QuadricStyle Nothing NoTextureCoordinates Outside fillStyle
                       fillStyle = if (fillMode == Filled) then FillStyle else SilhouetteStyle

        (Tx picIndex) -> do
                        texture Texture2D $= Enabled
                        bindTexture Texture2D (picList !! picIndex)
                        color (Color4 1.0 1.0 1.0 (1.0 :: GLfloat))
                        renderPrimitive Quads $ do
                            texCoord2 0.0 0.0;  vertex3 (-x) (-y) 0.0
                            texCoord2 1.0 0.0;  vertex3   x  (-y) 0.0
                            texCoord2 1.0 1.0;  vertex3   x    y  0.0
                            texCoord2 0.0 1.0;  vertex3 (-x)   y  0.0
                        texture Texture2D $= Disabled
                   where (sX,sY) = getGameObjectSize o
                         x = sX/2
                         y = sY/2
            
------------------------------------------
-- search routines
------------------------------------------

findObjectFromId :: GameObject t -> [(ObjectManager t)] -> GameObject t
findObjectFromId o mngs = findObjectFromIdAux (getGameObjectId o) (getGameObjectManagerName o) mngs

findObjectFromIdAux :: Integer -> String ->  [(ObjectManager t)] -> GameObject t
findObjectFromIdAux _ managerName [] = error ("Objects.findObjectFromIdAux error: object group " ++ managerName ++ " not found!")
findObjectFromIdAux objectId managerName (m:ms) | (managerName == getObjectManagerName m) = searchFromId objectId (getObjectManagerObjects m)
                     				| otherwise = findObjectFromIdAux objectId managerName ms

searchFromId :: Integer -> [(GameObject t)] -> GameObject t
searchFromId _ [] = error ("Objects.searchFromId error: object not found!")
searchFromId objectId (o:os) | (objectId == getGameObjectId o) = o
			     | otherwise = searchFromId objectId os


searchObjectManager :: String -> [(ObjectManager t)] -> ObjectManager t
searchObjectManager managerName [] = error ("Objects.searchObjectManager error: object group " ++ managerName ++ " not found!")
searchObjectManager managerName (m:ms) | (getObjectManagerName m == managerName) = m
                                       | otherwise = searchObjectManager managerName ms

searchGameObject :: String -> ObjectManager t -> GameObject t
searchGameObject objectName m = searchGameObjectAux objectName (getObjectManagerObjects m)

searchGameObjectAux :: String -> [(GameObject t)] -> GameObject t
searchGameObjectAux objectName [] = error ("Objects.searchGameObjectAux error: object " ++ objectName ++ " not found!")
searchGameObjectAux objectName (a:as) | (getGameObjectName a == objectName) = a
                                      | otherwise = searchGameObjectAux objectName as

------------------------------------------
-- update routines
------------------------------------------

-- substitutes an old object by a new one, given the function to be applied to the old object (whose id is given),
-- the name of its manager and the group of game managers.
updateObject :: (GameObject t -> GameObject t) -> Integer -> String -> [(ObjectManager t)] -> [(ObjectManager t)]
updateObject _ _ managerName [] = error ("Objects.updateObject error: object manager: " ++ managerName ++ " not found!")
updateObject f objectId managerName (m:ms) | (getObjectManagerName m == managerName) = (updateObjectManagerObjects newObjects m):ms
                                           | otherwise = m:(updateObject f objectId managerName ms)
                        		   where newObjects = updateObjectAux f objectId (getObjectManagerObjects m)

updateObjectAux :: (GameObject t -> GameObject t) -> Integer -> [(GameObject t)] -> [(GameObject t)]
updateObjectAux _ _ [] = error ("Objects.updateObjectAux error: object not found!")
updateObjectAux f objectId (o:os) | (getGameObjectId o == objectId) = (f o):os
                                  | otherwise = o:(updateObjectAux f objectId os)

------------------------------------------
-- moving routines
------------------------------------------

-- modifies all objects position according to their speed
moveGameObjects :: [(ObjectManager t)] -> [(ObjectManager t)]
moveGameObjects [] = []
moveGameObjects (m:ms) = (updateObjectManagerObjects (map moveSingleObject (getObjectManagerObjects m)) m):(moveGameObjects ms)

moveSingleObject :: GameObject t -> GameObject t
moveSingleObject o = if (getGameObjectAsleep o)
                        then o
                        else let (vX,vY) = getGameObjectSpeed o
                                 (pX,pY) = getGameObjectPosition o
                             in updateObjectPosition (pX + vX, pY + vY) o

------------------------------------------
-- destroy routines
------------------------------------------

destroyGameObject :: String -> String -> [(ObjectManager t)] -> [(ObjectManager t)]
destroyGameObject _ managerName [] = error ("Objects.destroyGameObject error: object manager: " ++ managerName ++ " not found!")
destroyGameObject objectName managerName (m:ms) | (getObjectManagerName m == managerName) = (updateObjectManagerObjects newObjects m):ms
                     | otherwise = m:(destroyGameObject objectName managerName ms)
                       where newObjects = destroyGameObjectAux objectName (getObjectManagerObjects m)

destroyGameObjectAux :: String -> [(GameObject t)] -> [(GameObject t)]
destroyGameObjectAux objectName [] = error ("Objects.destroyGameObjectAux error: object: " ++ objectName ++ " not found!") 
destroyGameObjectAux objectName (o:os) | (getGameObjectName o == objectName) = os
                    | otherwise = o:(destroyGameObjectAux objectName os)