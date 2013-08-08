{- 

worms - a very simple FunGEn example.
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Main where

import Text.Printf
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = GA Int Int Int (GLdouble,GLdouble) Int
data ObjectAttribute = NoObjectAttribute | Tail Int
data GameState = LevelStart Int | Level Int | GameOver
data TileAttribute = NoTileAttribute

type WormsAction a = IOGame GameAttribute ObjectAttribute GameState TileAttribute a
type WormsObject = GameObject ObjectAttribute
type WormsTile = Tile TileAttribute
type WormsMap = TileMatrix TileAttribute

tileSize, speedMod :: GLdouble
tileSize = 30.0
speedMod = 30.0

initPos, tail0Pos, tail1Pos :: (GLdouble,GLdouble)
initPos  = (45.0,105.0)
tail0Pos = (45.0,75.0)
tail1Pos = (45.0,45.0)

maxFood, initTailSize, defaultTimer :: Int
maxFood = 10
initTailSize = 2
defaultTimer = 10

magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("level1.bmp",	   Nothing),
           ("level2.bmp",	   Nothing),
           ("level3.bmp",          Nothing),
           ("gameover.bmp",        magenta),
           ("congratulations.bmp", magenta),
	   ("headn.bmp",	   magenta),
	   ("heads.bmp",	   magenta),
	   ("heade.bmp",	   magenta),
	   ("headw.bmp",	   magenta),
	   ("food.bmp",		   magenta),
	   ("segment.bmp",	   magenta),
	   ("border1.bmp",	   magenta),
	   ("border2.bmp",	   magenta),
	   ("border3.bmp",	   magenta),
	   ("free1.bmp",  	   magenta),
	   ("free2.bmp",	   magenta),
	   ("free3.bmp",	   magenta)]

-- position of the paths in the list:
border1, border2, border3, free1, free2, free3 :: Int
border1 = 11
border2 = 12
border3 = 13
free1   = 14
free2   = 15
free3   = 16

main :: IO ()
main = do
        let winConfig = ((100,50),(780,600),"WORMS - by Andre Furtado")

            gameMap = multiMap [(tileMap map1 tileSize tileSize),
                                (tileMap map2 tileSize tileSize),
                                (tileMap map3 tileSize tileSize)] 0

            gameAttribute = GA defaultTimer maxFood initTailSize initPos 0

            groups = [(objectGroup "messages"  createMsgs ),
                      (objectGroup "head"     [createHead]),
                      (objectGroup "food"     [createFood]),
                      (objectGroup "tail"      createTail )]

            input = [
                     (SpecialKey KeyLeft,  Press, turnLeft ),
                     (SpecialKey KeyRight, Press, turnRight),
                     (SpecialKey KeyUp,    Press, turnUp   ),
                     (SpecialKey KeyDown,  Press, turnDown )
                    ,(Char 'q',            Press, \_ _ -> funExit)
                    ]
        
        funInit winConfig gameMap groups (LevelStart 1) gameAttribute input gameCycle (Timer 150) bmpList

createMsgs :: [WormsObject]
createMsgs = let picLevel1          = Tex (150,50)  0
                 picLevel2          = Tex (150,50)  1
                 picLevel3          = Tex (150,50)  2
                 picGameOver        = Tex (300,100) 3
                 picCongratulations = Tex (300,100) 4
             in [(object "level1"          picLevel1          True (395,300) (0,0) NoObjectAttribute),
                 (object "level2"          picLevel2          True (395,300) (0,0) NoObjectAttribute),
                 (object "level3"          picLevel3          True (395,300) (0,0) NoObjectAttribute),
                 (object "gameover"        picGameOver        True (395,300) (0,0) NoObjectAttribute),
                 (object "congratulations" picCongratulations True (395,300) (0,0) NoObjectAttribute)]

createHead :: WormsObject
createHead = let pic = Tex (tileSize,tileSize) 5
             in object "head" pic True initPos (0,speedMod) NoObjectAttribute

createFood :: WormsObject
createFood = let pic = Tex (tileSize,tileSize) 9
             in object "food" pic True (0,0) (0,0) NoObjectAttribute

createTail :: [WormsObject]
createTail = let picTail = Tex (tileSize,tileSize) 10
             in  (object "tail0"  picTail False tail0Pos (0,0) (Tail 0)):
                 (object "tail1"  picTail False tail1Pos (0,0) (Tail 1)):
                 (createAsleepTails initTailSize (initTailSize + maxFood - 1) picTail)

createAsleepTails :: Int -> Int -> ObjectPicture -> [WormsObject]
createAsleepTails tMin tMax pic
    | (tMin > tMax) = []
    | otherwise = (object ("tail" ++ (show tMin)) pic True (0,0) (0,0) (Tail 0)):(createAsleepTails (tMin + 1) tMax pic)

turnLeft :: Modifiers -> Position -> WormsAction ()
turnLeft _ _ = do
    snakeHead <- findObject "head" "head"
    setObjectCurrentPicture 8 snakeHead
    setObjectSpeed (-speedMod,0) snakeHead
    
turnRight :: Modifiers -> Position -> WormsAction ()
turnRight _ _ = do
    snakeHead <- findObject "head" "head"
    setObjectCurrentPicture 7 snakeHead
    setObjectSpeed (speedMod,0) snakeHead

turnUp :: Modifiers -> Position -> WormsAction ()
turnUp _ _ = do
    snakeHead <- findObject "head" "head"
    setObjectCurrentPicture 5 snakeHead
    setObjectSpeed (0,speedMod) snakeHead

turnDown :: Modifiers -> Position -> WormsAction ()
turnDown _ _ = do
    snakeHead <- findObject "head" "head"
    setObjectCurrentPicture 6 snakeHead
    setObjectSpeed (0,-speedMod) snakeHead

gameCycle :: WormsAction ()
gameCycle = do
    (GA timer remainingFood tailSize previousHeadPos score) <- getGameAttribute
    gState <- getGameState
    case gState of
        LevelStart n -> case n of
                            4 -> do
                                congratulations <- findObject "congratulations" "messages"
                                drawObject congratulations
                                if (timer == 0)
                                    then funExit
                                    else (setGameAttribute (GA (timer - 1) remainingFood tailSize previousHeadPos score))
                            _ -> do
                                disableGameFlags
                                level <- findObject ("level" ++ (show n)) "messages"
                                drawObject level
                                if (timer == 0)
                                    then (do setGameState (Level n)
                                    	     enableGameFlags
                                             snakeHead <- findObject "head" "head"
                                             setObjectAsleep False snakeHead
                                  	     setObjectPosition initPos snakeHead
                                  	     setObjectSpeed (0.0,speedMod) snakeHead
                                  	     setObjectCurrentPicture 5 snakeHead
                                             setGameAttribute (GA defaultTimer remainingFood tailSize previousHeadPos score)
                                             destroyObject level
                                             setNewMap n)
                                    else setGameAttribute (GA (timer - 1) remainingFood tailSize previousHeadPos score)
        Level n -> do
                    if (remainingFood == 0) -- advance level!
                        then  (do setGameState (LevelStart (n + 1))
                                  resetTails
                                  disableGameFlags
                                  setGameAttribute (GA timer maxFood initTailSize initPos score))
                        else if (timer == 0) -- put a new food in the map
                               then (do food <- findObject "food" "food"
                                        newPos <- createNewFoodPosition
                                        setObjectPosition newPos food
                                        newFood <- findObject "food" "food"
                                        setObjectAsleep False newFood
                                        setGameAttribute (GA (-1) remainingFood tailSize previousHeadPos score)
                                        snakeHead <- findObject "head" "head"
                                        checkSnakeCollision snakeHead
                                        snakeHeadPosition <- getObjectPosition snakeHead
                                        moveTail snakeHeadPosition)
                               else if (timer > 0) -- there is no food in the map, so decrease the food timer
                                     then (do setGameAttribute (GA (timer - 1) remainingFood tailSize previousHeadPos score)
                                              snakeHead <- findObject "head" "head"
                                              checkSnakeCollision snakeHead
                                              snakeHeadPosition <- getObjectPosition snakeHead
                                              moveTail snakeHeadPosition)
                                     else (do -- there is a food in the map
                                        food <- findObject "food" "food"
                                        snakeHead <- findObject "head" "head"
                                        col <- objectsCollision snakeHead food
                                        if col
                                            then (do snakeHeadPosition <- getObjectPosition snakeHead
                                            	     setGameAttribute (GA defaultTimer (remainingFood-1) (tailSize + 1) snakeHeadPosition (score + 1))
                                                     addTail previousHeadPos
                                                     setObjectAsleep True food)
                                            else (do checkSnakeCollision snakeHead
                                            	     snakeHeadPosition <- getObjectPosition snakeHead
                                                     moveTail snakeHeadPosition))
                    showScore

        GameOver -> do
                        disableMapDrawing
                        gameover <- findObject "gameover" "messages"
                        drawMap
                        drawObject gameover
                        if (timer == 0)
                                then funExit
                                else (setGameAttribute (GA (timer - 1) 0 0 (0,0) 0))

showScore :: WormsAction ()
showScore = do
  (GA _ remainingFood _ _ score) <- getGameAttribute
  printOnScreen (printf "Score: %d    Food remaining: %d" score remainingFood) TimesRoman24 (40,8) 1.0 1.0 1.0
  showFPS TimesRoman24 (780-60,8) 1.0 0.0 0.0

setNewMap :: Int -> WormsAction ()
setNewMap 2 = setCurrentMapIndex 1
setNewMap 3 = setCurrentMapIndex 2
setNewMap _ = return ()

resetTails :: WormsAction ()
resetTails = do
        tail0 <- findObject "tail0" "tail"
        setObjectPosition tail0Pos tail0
        setObjectAttribute (Tail 0) tail0
        tail1 <- findObject "tail1" "tail"
        setObjectPosition tail1Pos tail1
        setObjectAttribute (Tail 1) tail1
        resetOtherTails initTailSize

resetOtherTails :: Int -> WormsAction ()
resetOtherTails n | (n == initTailSize + maxFood) = return ()
                  | otherwise = do tailn <- findObject ("tail" ++ (show n)) "tail"
                                   setObjectAsleep True tailn
                                   resetOtherTails (n + 1)

addTail :: (GLdouble,GLdouble) -> WormsAction ()
addTail presentHeadPos = do
        tails <- getObjectsFromGroup "tail"
        aliveTails <- getAliveTails tails []
        asleepTail <-  getAsleepTail tails
        setObjectAsleep False asleepTail
        setObjectPosition presentHeadPos asleepTail
        setObjectAttribute (Tail 0) asleepTail
        addTailNumber aliveTails

getAliveTails :: [WormsObject] -> [WormsObject] -> WormsAction [WormsObject]
getAliveTails [] t = return t
getAliveTails (o:os) t = do
	sleeping <- getObjectAsleep o
	if sleeping
		then getAliveTails os t
		else getAliveTails os (o:t)

getAsleepTail ::  [WormsObject] ->  WormsAction WormsObject
getAsleepTail [] = error "the impossible has happened!"
getAsleepTail (o:os) = do
	sleeping <- getObjectAsleep o
	if sleeping
		then return o
		else getAsleepTail os


addTailNumber :: [WormsObject] -> WormsAction ()
addTailNumber [] = return ()
addTailNumber (a:as) = do
        (Tail n) <- getObjectAttribute a
        setObjectAttribute (Tail (n + 1)) a
        addTailNumber as

moveTail :: (GLdouble,GLdouble) -> WormsAction ()
moveTail presentHeadPos = do
        (GA timer remainingFood tailSize previousHeadPos score) <- getGameAttribute
        tails <- getObjectsFromGroup "tail"
        aliveTails <- getAliveTails tails []
        lastTail <- findLastTail aliveTails
        setObjectPosition previousHeadPos lastTail
        setGameAttribute (GA timer remainingFood tailSize presentHeadPos score)
        changeTailsAttribute tailSize aliveTails

findLastTail :: [WormsObject] -> WormsAction WormsObject
findLastTail [] = error "the impossible has happened!"
findLastTail (t1:[]) = return t1
findLastTail (t1:t2:ts) = do (Tail na) <- getObjectAttribute t1
                             (Tail nb) <- getObjectAttribute t2
                             if (na > nb)
                                then findLastTail (t1:ts)
                                else findLastTail (t2:ts)

changeTailsAttribute :: Int -> [WormsObject] -> WormsAction ()
changeTailsAttribute _ [] = return ()
changeTailsAttribute tailSize (a:as) = do
        Tail n <- getObjectAttribute a
        setObjectAttribute (Tail (mod (n + 1) tailSize)) a
        changeTailsAttribute tailSize as

checkSnakeCollision :: WormsObject -> WormsAction ()
checkSnakeCollision snakeHead = do
                                headPos <- getObjectPosition snakeHead
                                tile <- getTileFromWindowPosition headPos
                                tails <- getObjectsFromGroup "tail"
                                col <- objectListObjectCollision tails snakeHead
                                if ( (getTileBlocked tile) || col)
                                        then (do setGameState GameOver
                                                 disableObjectsDrawing
                                                 disableObjectsMoving
                                                 setGameAttribute (GA defaultTimer 0 0 (0,0) 0))
                                        else return ()

createNewFoodPosition :: WormsAction (GLdouble,GLdouble)
createNewFoodPosition = do
                            x <- randomInt (1,18)
                            y <- randomInt (1,24)
                            mapPositionOk <- checkMapPosition (x,y)
                            tails <- getObjectsFromGroup "tail"
                            tailPositionNotOk <- pointsObjectListCollision (toPixelCoord y) (toPixelCoord x) tileSize tileSize tails
                            if (mapPositionOk && not tailPositionNotOk)
                                then (return (toPixelCoord y,toPixelCoord x))
                                else createNewFoodPosition
                            where toPixelCoord a = (tileSize/2) + (fromIntegral a) * tileSize

checkMapPosition :: (Int,Int) -> WormsAction Bool
checkMapPosition (x,y) = do
                            mapTile <- getTileFromIndex (x,y)
                            return (not (getTileBlocked mapTile))

b,f,g,h,i,j :: WormsTile
b = (border1, True,  0.0, NoTileAttribute)
f = (free1,   False, 0.0, NoTileAttribute)
g = (border2, True,  0.0, NoTileAttribute)
h = (free2,   False, 0.0, NoTileAttribute)
i = (border3, True,  0.0, NoTileAttribute)
j = (free3,   False, 0.0, NoTileAttribute)

map1 :: WormsMap
map1 = [  [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,b],
          [b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b]]

map2 :: WormsMap
map2 = [  [g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,g,g,g,g,g,g,g,g,g,g,g,g,g,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,g,g,g,g,g,g,g,g,g,g,g,g,g,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,h,g],
          [g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g,g]]

map3 :: WormsMap
map3 = [  [i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i],
          [i,j,j,j,j,j,j,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,i,i,i,i,i,i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,i,i,i,i,i,i,i,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,i,j,j,j,j,j,j,j,i],
          [i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i]]