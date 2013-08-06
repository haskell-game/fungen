-- {-# OPTIONS_HADDOCK hide #-}
{- |
GLUT-based keyboard/mouse handling.

Sven Panne 2000 <Sven.Panne@informatik.uni-muenchen.de>

This provides a "still down" event in addition to GLUT's key/mouse
button up/down events, and manages bindings from input events to actions.

-}

module Graphics.UI.GLUT.Input (
   Key(..), KeyEvent(..), KeyBinder, InputHandler, StillDownHandler, initGLUTInput
) where

import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.List(deleteBy)
import Graphics.UI.GLUT

---------------------------------------------------------------------------

data KeyEvent = Press | StillDown | Release   deriving Eq

---------------------------------------------------------------------------

-- | A mutable list of keys (or mouse buttons), along with modifier
-- state and mouse position.
type KeyTable = IORef [(Key, Modifiers, Position)]

newKeyTable :: IO KeyTable
newKeyTable = newIORef []

getKeys :: KeyTable -> IO [(Key, Modifiers, Position)]
getKeys = readIORef

insertIntoKeyTable :: KeyTable -> Key -> Modifiers -> Position -> IO ()
insertIntoKeyTable keyTab key mods pos = modifyIORef keyTab ((key,mods,pos):)

deleteFromKeyTable :: KeyTable -> Key -> IO ()
deleteFromKeyTable keyTab key = modifyIORef keyTab (deleteBy (\(k,_,_) (l,_,_) -> k==l) (key, nullmods, nullpos))
  where nullmods = Modifiers Up Up Up
        nullpos = Position 0 0

---------------------------------------------------------------------------

type InputHandler = Modifiers -> Position -> IO ()

type KeyBinder = Key -> KeyEvent -> Maybe InputHandler -> IO ()

-- TODO: Improve type 

-- | A mutable list of mappings from key/mousebutton up/down/stilldown
-- events to IO actions.
type BindingTable = IORef [((Key,KeyEvent), InputHandler)]

newBindingTable :: IO BindingTable
newBindingTable = newIORef []

bindKey :: BindingTable -> KeyBinder
bindKey bindingTable key event Nothing =
   modifyIORef bindingTable (\t -> [ e | e@(b,a) <- t, b /= (key, event)])
bindKey bindingTable key event (Just action) = do
   bindKey bindingTable key event Nothing
   modifyIORef bindingTable (((key, event), action) :)

execAction :: BindingTable -> Key -> KeyEvent -> Modifiers -> Position -> IO ()
execAction bindingTable key event mods pos  =
   readIORef bindingTable >>= (maybe (return ()) (\a -> a mods pos) . lookup (key, event))

---------------------------------------------------------------------------

type StillDownHandler = IO ()

stillDown :: BindingTable -> KeyTable -> StillDownHandler
stillDown bindingTable pressedKeys =
   getKeys pressedKeys >>= mapM_ (\(k,mods,pos) -> execAction bindingTable k StillDown mods pos)

---------------------------------------------------------------------------

-- | Initialise the input system, which keeps a list of input event to
-- action bindings and executes the the proper actions automatically.
-- Returns a function for adding bindings, and another which should be
-- called periodically (eg from refresh) to trigger still-down actions.
initGLUTInput :: IO (KeyBinder, StillDownHandler)
initGLUTInput = do
   -- Using "setKeyRepeat KeyRepeatOff" would be a little bit more
   -- efficient, but has two disadvantages: It is not yet implemented
   -- for M$ and it changes the global state of X11.
   globalKeyRepeat $= GlobalKeyRepeatOff
   bindingTable <- newBindingTable
   pressedKeys  <- newKeyTable
   let keyPress k mods pos = do
         insertIntoKeyTable pressedKeys k mods pos
         execAction bindingTable k Press mods pos
       keyRelease k mods pos = do 
         deleteFromKeyTable pressedKeys k
         execAction bindingTable k Release mods pos
       keyboardMouse k Down mods pos = keyPress   k mods pos
       keyboardMouse k Up   mods pos = keyRelease k mods pos
   keyboardMouseCallback $= Just keyboardMouse
   return (bindKey bindingTable, stillDown bindingTable pressedKeys)


