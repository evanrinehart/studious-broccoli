{-# LANGUAGE ExistentialQuantification #-}
module Widget where

import Rainbow
import Event
import Common

{-
data Widget = Widget
  { --wdgArea :: Float4
--  , depth :: Int -- auto computed from stacking order
--  , hidden :: Bool
--  , myID :: Int
    widgetRepaint :: IO (),
    widgetCanvas  :: Canvas,
    widgetActions :: UserActions (IO ()),
    widgetFinalize :: IO (),
    widgetArea :: Float4
  }
-}

data Widget = Widget
  { widgetCanvas    :: Canvas
  , widgetXY        :: Float2
  , widgetRepainter :: Repainter }

data Repainter = forall s . Repainter (IO s) (GFX -> Canvas -> s -> IO ())

someWidget :: Float2 -> Canvas -> IO a -> (GFX -> Canvas -> a -> IO ()) -> Widget
someWidget xy canvas getData repaint =
  Widget {
    widgetCanvas = canvas,
    widgetXY = xy,
    widgetRepainter = Repainter getData repaint
  }

widgetDimensions :: Widget -> Float4
widgetDimensions widg = F4 x y w h where
  F2 x y = widgetXY widg
  I2 wi hi = canvasWH (widgetCanvas widg)
  w = fromIntegral wi
  h = fromIntegral hi

repaintWidget :: GFX -> Widget -> IO ()
repaintWidget gfx w = case widgetRepainter w of
  Repainter getData repaint -> do
    x <- getData
    repaint gfx (widgetCanvas w) x

blitWidget :: GFX -> Widget -> IO ()
blitWidget gfx w = blitCanvas gfx (widgetCanvas w) (widgetXY w)




-- widget 
-- the field contains a collection of widgets. they define
-- canvas areas where (something) can generate graphics.

-- currently they are barely linked to actions. Some widget,
-- on construction, produce an action layer which shifts the
-- mouse motion coords according to the area. Finally, each
-- widget gives their actions to the master action and every
-- widget's actions get potentially executed by pollEvents
--   and the time driver.

-- widgets aren't really connected to data. But sometimes the
-- actions are created that cache state. So currently it's
-- awkward to access any data, not even widgets know about the
-- cache.

-- to support dynamic widgets, we can change things so widgets
-- are not so implicitly linked to data and actions?

-- things a widget needs other than basic area info and GL resources:
-- 1. a repaint method, and this method will want data sources
-- 2. that's basically it.

-- The Widget Layout. This is a top level structure that holds all
-- the widgets. This includes their dimensions, location, and
-- depth ordering. This thing can be modified (somehow). Using this
-- the main loop renders everything each frame. Widgets whose data
-- sources have updates should repaint their canvas. This has nothing
-- to do with actions. Though we don't know how to change the layout
-- yet. It would be nice to save a layout and restore it from disk.

-- Not A Widget. A controller or action set (UserActions (IO ())).
-- Simply execute all actions when a user event comes in. We moved
-- time passing into the user actions. So each controller potentially
-- causes something to animate. Controllers may contain references to
-- to cache variables and updaters. Controllers may also use data
-- sources for purposes. The controller set can be modified at runtime
-- (somehow) similar to the widget layout. This plan doesn't allow
-- actions to be blocked by a parallel controller. I.e. Top level
-- controllers are not nested in any way.

-- What a controller needs:
-- 1. IO action for each possible user input type, usually return ().
-- 2. access to any tools or resources necessary, such as data source.
--      so far we have been using controllers to generate data source!


-- Data Source. The core of any application. On one hand, data sources
-- provide up to date values to whatever wants them. On the other, the
-- data must be generated somehow, which is the central question here.
-- examples: the Ark game state, the material colors, the tapes, the
-- high score table, the GUI slider values, the mouse position, the
-- console buffer, the command line buffer.
