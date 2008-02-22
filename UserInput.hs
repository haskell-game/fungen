{-
   GLUT-based keyboard/mouse handling
   Sven Panne 2000.   mailto:Sven.Panne@informatik.uni-muenchen.de
-}

module UserInput (
   Key(..), KeyEvent(..), KeyBinder, StillDownHandler, initUserInput
) where

import IOExts(IORef, newIORef, readIORef, modifyIORef)
import List(delete)
import GLUT

---------------------------------------------------------------------------

data Key =
     KeyNormal  Char
   | KeySpecial SpecialKey
   | KeyMouse   MouseButton WindowPosition
   deriving Eq

data KeyEvent = Press | StillDown | Release   deriving Eq

---------------------------------------------------------------------------

type KeyTable = IORef [Key]

newKeyTable :: IO KeyTable
newKeyTable = newIORef []

getKeys :: KeyTable -> IO [Key]
getKeys = readIORef

insertIntoKeyTable :: KeyTable -> Key -> IO ()
insertIntoKeyTable keyTab key = modifyIORef keyTab (key:)

deleteFromKeyTable :: KeyTable -> Key -> IO ()
deleteFromKeyTable keyTab key = modifyIORef keyTab (delete key)

---------------------------------------------------------------------------

type KeyBinder = Key -> KeyEvent -> Maybe (IO ()) -> IO ()

-- TODO: Improve type
type BindingTable = IORef [((Key,KeyEvent), IO ())]

newBindingTable :: IO BindingTable
newBindingTable = newIORef []

bindKey :: BindingTable -> KeyBinder
bindKey bindingTable key event Nothing =
   modifyIORef bindingTable (\t -> [ e | e@(b,a) <- t, b /= (key, event)])
bindKey bindingTable key event (Just action) = do
   bindKey bindingTable key event Nothing
   modifyIORef bindingTable (((key, event), action) :)

execAction :: BindingTable -> Key -> KeyEvent -> IO ()
execAction bindingTable key event =
   readIORef bindingTable >>= (maybe (return ()) id . lookup (key, event))

---------------------------------------------------------------------------

type StillDownHandler = IO ()

stillDown :: BindingTable -> KeyTable -> StillDownHandler
stillDown bindingTable pressedKeys =
   getKeys pressedKeys >>= mapM_ (\k -> execAction bindingTable k StillDown)

---------------------------------------------------------------------------

initUserInput :: IO (KeyBinder, StillDownHandler)
initUserInput = do
   -- Using "setKeyRepeat KeyRepeatOff" would be a little bit more
   -- efficient, but has two disadvantages: It is not yet implemented
   -- for M$ and it changes the global state of X11.
   ignoreKeyRepeat True

   bindingTable <- newBindingTable
   pressedKeys  <- newKeyTable
   let keyPress   k = do insertIntoKeyTable pressedKeys k
                         execAction bindingTable k Press
       keyRelease k = do deleteFromKeyTable pressedKeys k
                         execAction bindingTable k Release

   keyboardFunc      (Just (\k _ -> keyPress   (KeyNormal  k)))
   keyboardUpFunc    (Just (\k _ -> keyRelease (KeyNormal  k)))
   specialFunc       (Just (\k _ -> keyPress   (KeySpecial k)))
   specialUpFunc     (Just (\k _ -> keyRelease (KeySpecial k)))
   mouseFunc         (Just (\k ud wp -> case ud of
                            Down -> keyPress   (KeyMouse   k wp)
                            Up   -> keyRelease (KeyMouse   k wp)))

   return (bindKey bindingTable, stillDown bindingTable pressedKeys)
