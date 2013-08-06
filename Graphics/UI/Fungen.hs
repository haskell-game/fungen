{- |
This is the main module of FunGEN (Functional Game Engine), which re-exports the rest.
-}
{- 

FunGEN - Functional Game Engine
http://joyful.com/fungen
Copyright (C)
2002  Andre Furtado <awbf@cin.ufpe.br>
2008, 2011-2013 Simon Michael <simon@joyful.com>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen (

  -- * Execution
  -- | Starting and stopping a game.
  module Graphics.UI.Fungen.Init,

  -- * Types
  -- | Some basic types.
  module Graphics.UI.Fungen.Types,

  -- * Images
  -- | Loading BMP image files.
  module Graphics.UI.Fungen.Loader,

  -- * Text
  -- | Printing text on the screen.
  module Graphics.UI.Fungen.Text,

  -- * Tile Maps
  -- | Tile maps (backgrounds).
  module Graphics.UI.Fungen.Map,

  -- * Objects
  -- | Game objects (sprites).
  module Graphics.UI.Fungen.Objects,

  -- * Input
  -- | User input from mouse and keyboard.
  module Graphics.UI.Fungen.Input,

  -- * Timing
  -- | Timing control.
  module Graphics.UI.Fungen.Timer,

  -- * Game
  -- | Game management.
  module Graphics.UI.Fungen.Game,

  -- * Display
  -- | Rendering the game window.
  module Graphics.UI.Fungen.Display,

  -- * Util
  -- | Miscellaneous utilities.
  module Graphics.UI.Fungen.Util

) where      

import Graphics.UI.Fungen.Types
import Graphics.UI.Fungen.Util
import Graphics.UI.Fungen.Loader
import Graphics.UI.Fungen.Objects
import Graphics.UI.Fungen.Map
import Graphics.UI.Fungen.Game
import Graphics.UI.Fungen.Display
import Graphics.UI.Fungen.Input
import Graphics.UI.Fungen.Timer
import Graphics.UI.Fungen.Text
import Graphics.UI.Fungen.Init