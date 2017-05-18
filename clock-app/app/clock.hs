{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (Applicative (..))
import Control.Concurrent.STM (TVar)
import Control.Monad (Monad (..), forever, mfilter)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Fixed (Pico)
import Data.Foldable (for_)
import Data.Function (($), (.), (&), flip)
import Data.Functor (Functor (..), (<$>), ($>), void)
import Data.Maybe (Maybe (..))
import Data.Time (TimeZone, UTCTime, TimeOfDay)
import Graphics.UI.Gtk (AttrOp (..), PangoRectangle (..))
import Prelude (String, Int, RealFrac(..), Fractional (..), Num (..), (^), fromIntegral)
import System.IO (IO)
import Text.Printf (printf)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.Time as Time
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango
import qualified Graphics.UI.Gtk as Gtk
import qualified System.Posix.Signals as Signals

main :: IO ()
main =
  void $
  STM.newTVarIO "" >>= \displayVar ->
  Gtk.initGUI *>
  Gtk.windowNew >>= \(window :: Gtk.Window) ->
  Gtk.windowSetDefaultSize window 300 100 *>
  Gtk.drawingAreaNew >>= \(drawingArea :: Gtk.DrawingArea) ->
  Gtk.set window
    [ Gtk.windowTitle := "Clock"
    , Gtk.containerChild := drawingArea
    ] *>
  Gtk.on window Gtk.deleteEvent (liftIO Gtk.mainQuit $> False) *>
  for_ [Signals.sigINT, Signals.sigTERM] (\s ->
    Signals.installHandler s (Signals.Catch (liftIO Gtk.mainQuit)) Nothing) *>
  createFontDescription >>= \(fontDescription :: Pango.FontDescription) ->
  Gtk.on drawingArea Gtk.draw (render displayVar fontDescription drawingArea) *>
  Gtk.widgetShowAll window *>
  Async.async (runClock displayVar) *>
  Async.async (watchClock displayVar drawingArea) *>
  Gtk.mainGUI

createFontDescription :: IO Pango.FontDescription
createFontDescription =
  Pango.fontDescriptionNew >>= \fd ->
  Pango.fontDescriptionSetFamily fd "Fira Mono" *>
  Pango.fontDescriptionSetSize fd 40 $>
  fd

render :: Gtk.WidgetClass w
  => TVar String -- ^ Variable containing the text to display
  -> Pango.FontDescription -- ^ Font to display the text in
  -> w -- ^ Widget we're rendering to (needed to get the size of it)
  -> Cairo.Render ()
render displayVar fontDescription widget =
  readTVarIO displayVar >>= \displayString ->
  liftIO (Gtk.widgetGetAllocation widget) >>= \(Gtk.Rectangle _ _ w h) ->
  Cairo.setSourceRGB 1 0.9 1 *>
  Cairo.paint *>
  Pango.createLayout displayString >>= \layout ->
  liftIO (Pango.layoutSetFontDescription layout (Just fontDescription)) *>
  liftIO (Pango.layoutSetAlignment layout Pango.AlignCenter) *>
  liftIO (Pango.layoutGetExtents layout) >>=
    \(_, (PangoRectangle x' y' x'' y'')) ->
  Cairo.moveTo ((fromIntegral w) / 2 - x'' / 2 - x')
               ((fromIntegral h) / 2 - y'' / 2 - y') *>
  Cairo.setSourceRGB 0.2 0.1 0.2 *>
  Pango.showLayout layout

-- | @'runClock' t@ is an IO action that runs forever, keeping the value of @t@
-- equal to the current time.
runClock :: TVar String -> IO ()
runClock displayVar =
  forever $
  getLocalTimeOfDay >>= \time ->
  properFraction (Time.todSec time)
    & \( clockSeconds :: Int
       , remainderSeconds :: Pico ) ->
  printf "%02d:%02d:%02d"
    (Time.todHour time) (Time.todMin time) clockSeconds & \s ->
  writeTVarIO displayVar s *>
  threadDelaySeconds (1 - remainderSeconds)

-- | Get the current time of day in the system time zone.
getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay =
  Time.getCurrentTimeZone >>= \(tz :: TimeZone) ->
  Time.getCurrentTime <&> \(utc :: UTCTime) ->
  Time.localTimeOfDay (Time.utcToLocalTime tz utc)

-- | @'watchClock' t w@ is an IO action that runs forever. Each time the value
-- of @t@ changes, it invalidates the drawing area @w@, thus forcing it to
-- redraw and update the display.
watchClock :: Gtk.WidgetClass w => TVar String -> w -> IO ()
watchClock displayVar drawingArea =
    go ""
  where
    go :: String -> IO ()
    go s =
      STM.atomically (mfilter (s /=) $ STM.readTVar displayVar) >>= \s' ->
      Gtk.postGUIAsync (invalidate drawingArea) *>
      go s'

-- | Invalidate (force the redrawing of) an entire widget.
invalidate :: Gtk.WidgetClass w => w -> IO ()
invalidate widget =
  Gtk.widgetGetAllocation widget >>= \(Gtk.Rectangle x y w h) ->
  Gtk.widgetQueueDrawArea widget x y w h

-- | Block for some fixed number of seconds.
threadDelaySeconds :: RealFrac n => n -> IO ()
threadDelaySeconds =
  Concurrent.threadDelay . round . (* million)

-- | One million = 10^6.
million :: Num n => n
million =
  10 ^ (6 :: Int)

-- | A convenient wrapper for 'STM.readTVar' that lifts the operation into any
-- monad supporting 'MonadIO'.
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO =
  liftIO . STM.atomically . STM.readTVar

-- | A convenient wrapper for 'STM.writeTVar' that lifts the operation into any
-- monad supporting 'MonadIO'.
writeTVarIO :: MonadIO m => TVar a -> a -> m ()
writeTVarIO v =
  liftIO . STM.atomically . STM.writeTVar v

-- | Equivalent to @flip '(<$>)'@.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip (<$>)
