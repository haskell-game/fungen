{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module contains some important game routines.

-}


module Fun_Game (
        Game, IOGame, runIOGame, runIOGameM, liftIOtoIOGame, liftIOtoIOGame',
        getGameState, setGameState,
        getGameFlags, setGameFlags,
        enableGameFlags, disableGameFlags,
        enableMapDrawing, disableMapDrawing,
        enableObjectsDrawing, disableObjectsDrawing,
        enableObjectsMoving, disableObjectsMoving,
        getObjectManagers, setObjectManagers,
        getGameAttribute, setGameAttribute,
        createGame, funExit,
        drawMap, clearScreen, getTileFromIndex, getTileFromWindowPosition, setCurrentMapIndex,
        drawAllObjects, drawObject, moveAllObjects, destroyObjects, destroyObject,
        getObjectsFromGroup, addObjectsToGroup, addObjectsToNewGroup, findObjectManager,findObject,
        getObjectName, getObjectGroupName, getObjectAsleep, getObjectSize,
        getObjectPosition, getObjectSpeed, getObjectAttribute,
        setObjectPosition, setObjectAsleep, setObjectSpeed, setObjectCurrentPicture, setObjectAttribute,
        replaceObject,
        reverseXSpeed, reverseYSpeed,
        objectsCollision, objectsFutureCollision,
        objectListObjectCollision, objectListObjectFutureCollision,
        objectTopMapCollision, objectBottomMapCollision, objectRightMapCollision, objectLeftMapCollision,
        pointsObjectCollision, pointsObjectListCollision,
        objectTopMapFutureCollision, objectBottomMapFutureCollision, objectRightMapFutureCollision, objectLeftMapFutureCollision,
        printOnPrompt, printOnScreen, printText, randomFloat, randomInt, randomDouble,
        showFPS,
        wait
) where

import Fun_Types
import Fun_Aux
import Fun_Loader
import Fun_Text
import Fun_Map
import Fun_Objects
import GL
import GLU
import GLUT
import IOExts
import Monad
foreign import shutdownHaskellAndExit :: Int -> IO ()



data Game t s u v = Game {
	gameMap       :: IORef (GameMap v),
    	gameState     :: IORef u,
	gameFlags     :: IORef GameFlags,
	objManagers   :: IORef [(ObjectManager s)],
	textList      :: IORef [Text],
	quadricObj    :: QuadricObj,
	windowConfig  :: IORef WindowConfig,
	gameAttribute :: IORef t,
	pictureList   :: IORef [TextureName],
	fpsInfo       :: IORef (Int,Int,Float)  -- only for debugging
	}

newtype IOGame t s u v a = IOG (Game  t s u v -> IO (Game t s u v,a))
type GameFlags = (Bool,Bool,Bool) -- mapDrawing, objectsDrawing, objectsMoving

----------------------------------
-- IOGame Monad definitions
----------------------------------
-- OBS.: all of this stuff is done to encapsulate Monad IO inside
--       Monad IOGame. Thanks to Jay Cox who suggested this solution.

bindST :: IOGame t s u v a -> (a -> IOGame t s u v b) -> IOGame t s u v b
bindST (IOG x) f =
   IOG (\s -> ( x s >>= \(s',v) -> let IOG g = f v in g s'))

unitST :: a -> IOGame t s u v a
unitST v = IOG (\s -> return (s,v))

instance Monad (IOGame t s u v) where
  (>>=) = bindST
  return = unitST

runIOGame :: IOGame t s u v a -> Game t s u v -> IO (Game t s u v,a)  -- (a,Game t s u v) the state tuple
runIOGame (IOG f) g = f g

runIOGameM :: IOGame t s u v a -> Game t s u v -> IO ()
runIOGameM x g = runIOGame x g >> return ()
                 
liftIOtoIOGame      :: IO a -> IOGame t s u v a
liftIOtoIOGame p    =
     IOG $ \s -> (do y <- p
                     return (s,y))
                     
liftIOtoIOGame'     :: (a -> IO ()) -> a -> IOGame t s u v ()
liftIOtoIOGame' p q =
     IOG $ \s -> (do p q
                     return (s,()))

----------------------------------
-- get & set routines
----------------------------------

getMap :: IOGame t s u v (GameMap v)
getMap = IOG ( \game -> (readIORef (gameMap game) >>= \gm -> if (isMultiMap gm)
                                                                then (return (game,getCurrentMap gm))
                                                                else (return (game,gm)) ))

getRealMap :: IOGame t s u v (GameMap v)
getRealMap = IOG ( \game -> (readIORef (gameMap game) >>= \gm -> (return (game,gm)) ))

setRealMap :: GameMap v -> IOGame t s u v ()
setRealMap m = IOG ( \game -> (writeIORef (gameMap game) m >> return (game,()) ))


getGameState :: IOGame t s u v u
getGameState = IOG ( \game -> (readIORef (gameState game) >>= \gs -> return (game,gs) ))

setGameState :: u -> IOGame t s u v ()
setGameState s = IOG ( \game -> (writeIORef (gameState game) s >> return (game,()) ))

getTextList :: IOGame t s u v [Text]
getTextList = IOG ( \game -> (readIORef (textList game) >>= \tl -> return (game,tl) ))

setTextList :: [Text] -> IOGame t s u v ()
setTextList t = IOG ( \game -> (writeIORef (textList game) t >> return (game,()) ))

getGameFlags :: IOGame t s u v GameFlags
getGameFlags = IOG ( \game -> (readIORef (gameFlags game) >>= \gf -> return (game,gf) ))

setGameFlags :: GameFlags -> IOGame t s u v ()
setGameFlags f = IOG ( \game -> (writeIORef (gameFlags game) f >> return (game,()) ))

getObjectManagers :: IOGame t s u v [(ObjectManager s)]
getObjectManagers = IOG ( \game -> (readIORef (objManagers game) >>= \om -> return (game,om) ))

setObjectManagers :: [(ObjectManager s)] -> IOGame t s u v ()
setObjectManagers o = IOG ( \game -> (writeIORef (objManagers game) o >> return (game,()) ))
                                                   
getQuadric :: IOGame t s u v QuadricObj
getQuadric = IOG ( \game -> return (game,quadricObj game) )

getPictureList :: IOGame t s u v [TextureName]
getPictureList = IOG ( \game -> (readIORef (pictureList game) >>= \pl -> return (game,pl) ))

getWindowConfig :: IOGame t s u v WindowConfig
getWindowConfig = IOG ( \game -> (readIORef (windowConfig game) >>= \wc -> return (game,wc) ))

getGameAttribute :: IOGame t s u v t
getGameAttribute = IOG ( \game -> (readIORef (gameAttribute game) >>= \ga -> return (game,ga) ))

setGameAttribute :: t -> IOGame t s u v ()
setGameAttribute ga = IOG ( \game -> (writeIORef (gameAttribute game) ga >> return (game,()) ))

-- internal use only
getFpsInfo :: IOGame t s u v (Int,Int,Float)
getFpsInfo = IOG ( \game -> (readIORef (fpsInfo game) >>= \fpsi -> return (game,fpsi) ))

-- internal use only
setFpsInfo :: (Int,Int,Float) -> IOGame t s u v ()
setFpsInfo f = IOG ( \game -> (writeIORef (fpsInfo game) f >> return (game,()) ))
                                                   
----------------------------------
-- initialization of the game
----------------------------------
createGame :: GameMap v -> [(ObjectManager s)] -> WindowConfig -> u -> t -> FilePictureList -> IO (Game t s u v)
createGame gMap objectManagers winConf gState gAttrib filePicList = do
        gM <- newIORef gMap
        gS <- newIORef gState
        gF <- newIORef (True,True,True)
        gO <- newIORef objectManagers
        gT <- newIORef []
        gQ <- newQuadric
        gW <- newIORef winConf
        gA <- newIORef gAttrib
        picList <- loadPictures filePicList
        gP <- newIORef picList
        gFPS <- newIORef (0,0,0.0)
        return (Game {
            gameMap       = gM,
            gameState     = gS,
            gameFlags     = gF,
            objManagers   = gO,
            textList      = gT,
            quadricObj    = gQ,
            windowConfig  = gW,
            gameAttribute = gA,
            pictureList   = gP,
            fpsInfo 	  = gFPS
            })

-- loads all of the pictures used in the game
loadPictures :: [(FilePath,InvList)] -> IO [TextureName]
loadPictures pathsAndInvLists = do
        bmps <- loadBitmapList (map pathAndInv2color3List pathsAndInvLists)
        texBmList <- (genTextures (length bmps))
        texStuff texBmList bmps
        return texBmList

---------------------------------
-- ending the game
---------------------------------
funExit :: IOGame t s u v ()
funExit = liftIOtoIOGame' shutdownHaskellAndExit 1

----------------------------------
-- map routines
----------------------------------

-- draws the background map
drawMap :: IOGame t s u v ()
drawMap = do
    m <- getMap
    p <- getPictureList
    (_,(winWidth, winHeight),_) <- getWindowConfig
    liftIOtoIOGame $ drawGameMap m (fromIntegral winWidth, fromIntegral winHeight) p

-- returns a mapTile, given its pixel position (x,y) in the screen
getTileFromWindowPosition :: (Double,Double) -> IOGame t s u v (Tile v)
getTileFromWindowPosition (preX,preY) = do
       m <- getMap
       if (isTileMap m)
            then let (tileXsize,tileYsize) = getTileMapTileSize m
                     (scrollX,scrollY) = getTileMapScroll m
                     (x,y) = (preX + scrollX,preY + scrollY)
                     (sX,sY) = getTileMapSize m
                 in if (x >= sX || y >= sY)
                        then error ("Fun_Game.getTileFromWindowPosition error: pixel " ++ (show (x,y)) ++ " out of map range!")
                        else getTileFromIndex (fromEnum (y/tileXsize),fromEnum (x/tileYsize)) -- (x,y) window orientation is different than index (x,y)!
            else error "Fun_Game.getTileFromWindowPosition error: game map is not a tile map!"

-- returns a mapTile, given its index (x,y) in the tile map
getTileFromIndex :: (Int,Int) -> IOGame t s u v (Tile v)
getTileFromIndex (x,y) = do
        m <- getMap
        if (isTileMap m)
            then let matrix = getTileMapTileMatrix m
                     (mapX,mapY) = matrixSize matrix
                 in if (mapX >= x && mapY >= y && x >= 0 && y >= 0)
                        then return ( (matrix !! (mapX - x)) !! y)
                        else error ("Fun_Game.getTileFromIndex error: tile index " ++ (show (x,y)) ++ " out of map range!")
            else error "Fun_Game.getTileFromIndex error: game map is not a tile map!"

-- paint the whole screen with a specified RGB color
clearScreen :: Float -> Float -> Float -> IOGame t s u v ()
clearScreen r g b = liftIOtoIOGame $ clearGameScreen r g b

-- set the current map for a MultiMap
setCurrentMapIndex :: Int -> IOGame t s u v ()
setCurrentMapIndex i = do
        m <- getRealMap
        if (isMultiMap m)
                then (setRealMap (updateCurrentIndex m i))
                else (error "Fun_Game.setCurrentMapIndex error: you are not working with MultiMaps!")

----------------------------------
-- flags routines
----------------------------------

enableGameFlags :: IOGame t s u v ()
enableGameFlags = setGameFlags (True,True,True)

disableGameFlags :: IOGame t s u v ()
disableGameFlags = setGameFlags (False,False,False)

enableMapDrawing :: IOGame t s u v ()
enableMapDrawing = do
    (_,od,om) <- getGameFlags
    setGameFlags (True,od,om)

disableMapDrawing :: IOGame t s u v ()
disableMapDrawing = do
    (_,od,om) <- getGameFlags
    setGameFlags (False,od,om)

enableObjectsDrawing :: IOGame t s u v ()
enableObjectsDrawing = do
    (md,_,om) <- getGameFlags
    setGameFlags (md,True,om)

disableObjectsDrawing :: IOGame t s u v ()
disableObjectsDrawing = do
    (md,_,om) <- getGameFlags
    setGameFlags (md,False,om)

enableObjectsMoving :: IOGame t s u v ()
enableObjectsMoving = do
    (md,od,_) <- getGameFlags
    setGameFlags (md,od,True)

disableObjectsMoving :: IOGame t s u v ()
disableObjectsMoving = do
    (md,od,_) <- getGameFlags
    setGameFlags (md,od,False)

----------------------------------
-- objects routines
----------------------------------

-- draws all visible objects
drawAllObjects :: IOGame t s u v ()
drawAllObjects = do
    o <- getObjectManagers
    q <- getQuadric
    p <- getPictureList
    liftIOtoIOGame $ drawGameObjects o q p

-- draw one object
drawObject :: GameObject s -> IOGame t s u v ()
drawObject o = do
    q <- getQuadric
    p <- getPictureList
    liftIOtoIOGame $ drawGameObject o q p

-- changes objects position according to its speed
moveAllObjects :: IOGame t s u v ()
moveAllObjects = do
    m <- getObjectManagers
    let newManagers = moveGameObjects m
    setObjectManagers newManagers

-- destroys objects from the game
destroyObjects :: [(GameObject s)] -> IOGame t s u v ()
destroyObjects [] = return ()
destroyObjects (o:os) = destroyObject o >> destroyObjects os

-- destroys an object from the game
destroyObject :: GameObject s -> IOGame t s u v ()
destroyObject obj = do
    m <- getObjectManagers
    let objName = getGameObjectName obj
    mngName <- getObjectGroupName obj
    let newManagers = destroyGameObject objName mngName m
    setObjectManagers newManagers

-- returns the list of all objects from the group whose name is given
getObjectsFromGroup :: String -> IOGame t s u v [(GameObject s)]
getObjectsFromGroup mngName = do
        mng <- findObjectManager mngName
        return (getObjectManagerObjects mng)

-- adds an object to a previously created group
addObjectsToGroup :: [(GameObject s)] -> String -> IOGame t s u v ()
addObjectsToGroup objs managerName = do
	manager <- findObjectManager managerName
	managers <- getObjectManagers
	let newManagers = addObjectsToManager objs managerName managers 
	setObjectManagers newManagers

-- adds an object to a new group
addObjectsToNewGroup :: [(GameObject s)] -> String -> IOGame t s u v ()
addObjectsToNewGroup objs newMngName = do
	let newManager = objectGroup newMngName objs
	managers <- getObjectManagers
	setObjectManagers (newManager:managers)

-- returns an object manager of the game, given its name (internal use)
findObjectManager :: String -> IOGame t s u v (ObjectManager s)
findObjectManager mngName = do
    objectManagers <- getObjectManagers
    return (searchObjectManager mngName objectManagers)
    
-- returns an object of the game, given its name and is object manager name
findObject :: String -> String -> IOGame t s u v (GameObject s)
findObject objName mngName = do
    objectManagers <- getObjectManagers
    let m = searchObjectManager mngName objectManagers
    return (searchGameObject objName m)

-- there is no need to search through the managers, because the name of an object is
-- never modified so the result of this function will always be safe.
getObjectName :: GameObject s -> IOGame t s u v String
getObjectName o = return (getGameObjectName o)

-- because an object can have its group (manager) name modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectGroupName :: GameObject s -> IOGame t s u v String
getObjectGroupName o = do managers <- getObjectManagers
                          let obj = findObjectFromId o managers
                          return (getGameObjectManagerName obj)

-- because an object can have its sleeping status modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectAsleep :: GameObject s -> IOGame t s u v Bool
getObjectAsleep o = do managers <- getObjectManagers
                       let obj = findObjectFromId o managers
                       return (getGameObjectAsleep obj)

-- because an object can have its size modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectSize :: GameObject s -> IOGame t s u v (Double,Double)
getObjectSize o = do managers <- getObjectManagers
                     let obj = findObjectFromId o managers
                     return (getGameObjectSize obj)

-- because an object can have its position modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectPosition :: GameObject s -> IOGame t s u v (Double,Double)
getObjectPosition o = do managers <- getObjectManagers
                         let obj = findObjectFromId o managers
                         return (getGameObjectPosition obj)

-- because an object can have its speed modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectSpeed :: GameObject s -> IOGame t s u v (Double,Double)
getObjectSpeed o = do managers <- getObjectManagers
                      let obj = findObjectFromId o managers
                      return (getGameObjectSpeed obj)

-- because an object can have its attribute modified, it is necessary
-- to search through the managers to find it, otherwise this functions won't be safe.
getObjectAttribute :: GameObject s -> IOGame t s u v s
getObjectAttribute o = do managers <- getObjectManagers
                          let obj = findObjectFromId o managers
                          return (getGameObjectAttribute obj)

-- changes the sleeping status of an object, given its new status
setObjectAsleep :: Bool -> GameObject s -> IOGame t s u v ()
setObjectAsleep asleep obj = replaceObject obj (updateObjectAsleep asleep)

-- changes the position of an object, given its new position
setObjectPosition :: (Double,Double) -> GameObject s -> IOGame t s u v ()
setObjectPosition pos obj = replaceObject obj (updateObjectPosition pos)

-- changes the speed of an object, given its new speed
setObjectSpeed :: (Double,Double) -> GameObject s -> IOGame t s u v ()
setObjectSpeed speed obj = replaceObject obj (updateObjectSpeed speed)

-- changes the current picture of a multitextured object
setObjectCurrentPicture :: Int -> GameObject s -> IOGame t s u v ()
setObjectCurrentPicture n obj = do
	picList <- getPictureList
	replaceObject obj (updateObjectPicture n ((length picList) - 1))

-- changes the attribute of an object, given its new attribute
setObjectAttribute :: s -> GameObject s -> IOGame t s u v ()
setObjectAttribute a obj = replaceObject obj (updateObjectAttribute a)

-- replaces an object by a new one, given the old object and the function that must be applied to it.
replaceObject :: GameObject s -> (GameObject s -> GameObject s) -> IOGame t s u v ()
replaceObject obj f = do
    managerName <- getObjectGroupName obj
    oldManagers <- getObjectManagers
    let objectId = getGameObjectId obj
        newManagers = updateObject f objectId managerName oldManagers
    setObjectManagers newManagers

reverseXSpeed :: GameObject s -> IOGame t s u v ()
reverseXSpeed  o = do (vX,vY) <- getObjectSpeed o
                      setObjectSpeed (-vX,vY) o

reverseYSpeed :: GameObject s -> IOGame t s u v ()
reverseYSpeed o = do (vX,vY) <- getObjectSpeed o
                     setObjectSpeed (vX,-vY) o

-----------------------
-- collision routines
-----------------------

-- checks the collision between an object and the top of the map
objectTopMapCollision :: GameObject s -> IOGame t s u v Bool
objectTopMapCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do m <- getMap
                (_,pY) <- getObjectPosition o
                (_,sY) <- getObjectSize o
                let (_,mY) = getMapSize m
                return (pY + (sY/2) > (realToFrac mY))

-- checks the collision between an object and the top of the map in the next game cicle
objectTopMapFutureCollision :: GameObject s -> IOGame t s u v Bool
objectTopMapFutureCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do m <- getMap
                (_,pY) <- getObjectPosition o
                (_,vY) <- getObjectSpeed o
                (_,sY) <- getObjectSize o
                let (_,mY) = getMapSize m
                return (pY + (sY/2) + vY > (realToFrac mY))

-- checks the collision between an object and the bottom of the map
objectBottomMapCollision :: GameObject s -> IOGame t s u v Bool
objectBottomMapCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do (_,pY) <- getObjectPosition o
                (_,sY) <- getObjectSize o
                return (pY - (sY/2) < 0)

-- checks the collision between an object and the bottom of the map in the next game cicle
objectBottomMapFutureCollision :: GameObject s -> IOGame t s u v Bool
objectBottomMapFutureCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do (_,pY) <- getObjectPosition o
                (_,sY) <- getObjectSize o
                (_,vY) <- getObjectSpeed o
                return (pY - (sY/2) + vY < 0)

-- checks the collision between an object and the right side of the map
objectRightMapCollision :: GameObject s -> IOGame t s u v Bool
objectRightMapCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do m <- getMap
                (pX,_) <- getObjectPosition o
                (sX,_) <- getObjectSize o
                let (mX,_) = getMapSize m
                return (pX + (sX/2) > (realToFrac mX))

-- checks the collision between an object and the right side of the map in the next game cicle
objectRightMapFutureCollision :: GameObject s -> IOGame t s u v Bool
objectRightMapFutureCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do m <- getMap
                (pX,_) <- getObjectPosition o
                (sX,_) <- getObjectSize o
                (vX,_) <- getObjectSpeed o
                let (mX,_) = getMapSize m
                return (pX + (sX/2) + vX > (realToFrac mX))

-- checks the collision between an object and the left side of the map
objectLeftMapCollision :: GameObject s -> IOGame t s u v Bool
objectLeftMapCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do (pX,_) <- getObjectPosition o
                (sX,_) <- getObjectSize o
                return (pX - (sX/2) < 0)

-- checks the collision between an object and the left side of the map in the next game cicle
objectLeftMapFutureCollision :: GameObject s -> IOGame t s u v Bool
objectLeftMapFutureCollision o = do
    asleep <- getObjectAsleep o
    if asleep
        then (return False)
        else do (pX,_) <- getObjectPosition o
                (sX,_) <- getObjectSize o
                (vX,_) <- getObjectSpeed o
                return (pX - (sX/2) + vX < 0)

-- checks the collision between two objects
objectsCollision :: GameObject s -> GameObject s -> IOGame t s u v Bool
objectsCollision o1 o2 = do
    asleep1 <- getObjectAsleep o1
    asleep2 <- getObjectAsleep o2
    if (asleep1 || asleep2)
                then (return False)
                else  do (p1X,p1Y) <- getObjectPosition o1
                         (p2X,p2Y) <- getObjectPosition o2
                         (s1X,s1Y) <- getObjectSize o1
                         (s2X,s2Y) <- getObjectSize o2
        
                         let aX1 = p1X - (s1X/2)
                             aX2 = p1X + (s1X/2)
                             aY1 = p1Y - (s1Y/2)
                             aY2 = p1Y + (s1Y/2)
        
                             bX1 = p2X - (s2X/2)
                             bX2 = p2X + (s2X/2)
                             bY1 = p2Y - (s2Y/2)
                             bY2 = p2Y + (s2Y/2)
                              
                         return ((bX1 < aX2) && (aX1 < bX2) && (bY1 < aY2) && (aY1 < bY2))

-- checks the collision between two objects in the next game cicle
objectsFutureCollision :: GameObject s -> GameObject s -> IOGame t s u v Bool
objectsFutureCollision o1 o2 = do
    asleep1 <- getObjectAsleep o1
    asleep2 <- getObjectAsleep o2
    if (asleep1 || asleep2)
                then (return False)
                else do (p1X,p1Y) <- getObjectPosition o1
                        (p2X,p2Y) <- getObjectPosition o2
                        (v1X,v1Y) <- getObjectSpeed o1
                        (v2X,v2Y) <- getObjectSpeed o2
                        (s1X,s1Y) <- getObjectSize o1
                        (s2X,s2Y) <- getObjectSize o2
        
                        let aX1 = p1X - (s1X/2) + v1X
                            aX2 = p1X + (s1X/2) + v1X
                            aY1 = p1Y - (s1Y/2) + v1Y
                            aY2 = p1Y + (s1Y/2) + v1Y

                            bX1 = p2X - (s2X/2) + v2X
                            bX2 = p2X + (s2X/2) + v2X
                            bY1 = p2Y - (s2Y/2) + v2Y
                            bY2 = p2Y + (s2Y/2) + v2Y
                        return ((bX1 < aX2) && (aX1 < bX2) && (bY1 < aY2) && (aY1 < bY2))

objectListObjectCollision :: [(GameObject s)] -> GameObject s -> IOGame t s u v Bool
objectListObjectCollision [] _ = return False
objectListObjectCollision (a:as) b = do
        col <- objectsCollision a b
        if col
                then (return True)
                else (objectListObjectCollision as b)

objectListObjectFutureCollision :: [(GameObject s)] -> GameObject s -> IOGame t s u v Bool
objectListObjectFutureCollision [] _ = return False
objectListObjectFutureCollision (a:as) b = do
        col <- objectsFutureCollision a b
        if col
                then (return True)
                else (objectListObjectFutureCollision as b)

pointsObjectCollision :: Double -> Double -> Double -> Double -> GameObject s -> IOGame t s u v Bool
pointsObjectCollision p1X p1Y s1X s1Y o2 = do
        asleep <- getObjectAsleep o2
        if asleep
                then (return False)
                else do (p2X,p2Y) <- getObjectPosition o2
                        (s2X,s2Y) <- getObjectSize o2

                        let aX1 = p1X - (s1X/2)
                            aX2 = p1X + (s1X/2)
                            aY1 = p1Y - (s1Y/2)
                            aY2 = p1Y + (s1Y/2)
        
                            bX1 = p2X - (s2X/2)
                            bX2 = p2X + (s2X/2)
                            bY1 = p2Y - (s2Y/2)
                            bY2 = p2Y + (s2Y/2)
                        return ((bX1 < aX2) && (aX1 < bX2) && (bY1 < aY2) && (aY1 < bY2))
                         
pointsObjectListCollision :: Double -> Double -> Double -> Double -> [(GameObject s)] -> IOGame t s u v Bool
pointsObjectListCollision _ _ _ _ [] = return False
pointsObjectListCollision p1X p1Y s1X s1Y (o:os) = do
        col <- pointsObjectCollision p1X p1Y s1X s1Y o
        if col
                then (return True)
                else (pointsObjectListCollision p1X p1Y s1X s1Y os)

-----------------------------------------------
--              TEXT ROUTINES                --
-----------------------------------------------
-- prints a string in the prompt
printOnPrompt :: Show a => a -> IOGame t s u v ()
printOnPrompt a = liftIOtoIOGame' print a

-- prints a string in the current window
printOnScreen :: String -> BitmapFont -> (Double,Double) -> Float -> Float -> Float -> IOGame t s u v ()
printOnScreen text font pos r g b = do
        t <- getTextList
        setTextList ([(text,font,pos,r,g,b)] ++ t)

-- internal use of the engine
printText :: IOGame t s u v ()
printText = do
        t <- getTextList
        liftIOtoIOGame $ putGameText t
        setTextList []

-----------------------------------------------
--     RANDOM NUMBER GENERATOR ROUTINES      --
-----------------------------------------------
randomInt :: (Int,Int) -> IOGame t s u v Int
randomInt (x,y) = liftIOtoIOGame $ randInt (x,y)

randomFloat :: (Float,Float) -> IOGame t s u v Float
randomFloat (x,y) = liftIOtoIOGame $ randFloat (x,y)

randomDouble :: (Double,Double) -> IOGame t s u v Double
randomDouble (x,y) = liftIOtoIOGame $ randDouble (x,y)

-----------------------------------------------
--           DEBUGGING ROUTINES              --
-----------------------------------------------

-- shows the frame rate (or frame per seconds) 
showFPS :: BitmapFont -> (Double,Double) -> Float -> Float -> Float -> IOGame t s u v ()
showFPS font pos r g b = do
	(framei,timebasei,fps) <- getFpsInfo
	timei <- getElapsedTime
	let frame = (toEnum (framei + 1)) :: Float
	    timebase = (toEnum timebasei) :: Float
	    time = (toEnum timei) :: Float
	if (timei - timebasei > 1000)
		then setFpsInfo (0,timei,(frame*(toEnum 1000)/(time-timebase)))
		else setFpsInfo ((framei + 1),timebasei,fps)
	printOnScreen (show fps) font pos r g b

-- get the elapsed time of the game
getElapsedTime :: IOGame t s u v Int
getElapsedTime = liftIOtoIOGame $ get ElapsedTime

wait :: Int -> IOGame t s u v ()
wait delay = do
	printText 				    -- force text messages to be printed (is not working properly!)
	(framei,timebasei,fps) <- getFpsInfo
	setFpsInfo (framei,(timebasei + delay),fps) -- helps FPS info to be displayed correctly (if requested)
	startTime <- getElapsedTime
	
	waitAux delay startTime

waitAux :: Int -> Int -> IOGame t s u v ()
waitAux delay startTime = do
	presentTime <- getElapsedTime
	if (presentTime - startTime > delay)
		then return ()
		else waitAux delay startTime
	