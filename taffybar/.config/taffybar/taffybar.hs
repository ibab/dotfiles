import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS2
import System.Taffybar.Battery
import System.Taffybar.Maildir
import System.Taffybar.Pager
import System.Taffybar.TaffyPager

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery

import qualified Data.Time.Clock as Clock
--import System.Locale

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Time.Format
import Data.Time.LocalTime

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

memCfg = defaultGraphConfig
  { graphDataColors = [(22/255, 147/255, 165/255, 1)]
  , graphLabel = Nothing
  , graphWidth = 40
  , graphPadding = 3
  , graphBorderColor = (0.247, 0.247, 0.247)
  }

cpuCfg = defaultGraphConfig
  { graphDataColors = [(251/255, 184/255, 41/255, 1), (1, 0, 1, 0.5)]
  , graphLabel = Nothing
  , graphWidth = 40
  , graphPadding = 3
  , graphBorderColor = (0.247, 0.247, 0.247)
  }

batCfg = defaultBatteryConfig
  { barPadding     = 3
  , barColor       = \perc -> let (RGB r g b) =  hsl (120 * perc) 1 0.5 in (r, g, b)
  , barBorderColor = (0.247, 0.247, 0.247)
  , barWidth = 14
  }

-- Data.Time.Format doesn't allow customizing the timezone offset,
-- so I'm implementing the clock manually.
-- (I also don't need the pop-up calendar of the default clock widget)
myClock :: IO Widget
myClock = do
  z <- getCurrentTimeZone
  l <- pollingLabelNew "" 1.0 (callback z)
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  widgetShowAll ebox
  return (toWidget ebox)
  where
    callback z = do
      time <- showFormat z "%a %Y-%m-%d<span fgcolor='grey'>T</span><span fgcolor='yellow'>%H:%M</span>"
      os <- offset z
      return $ time ++ "<span fgcolor='grey'>" ++ os ++ "</span>"
    offset z = do
      (x:y:z:xs) <- showFormat z "%z"
      return (x:y:z:':':xs)
    showFormat z f = return . formatTime defaultTimeLocale f . utcToZonedTime z =<< Clock.getCurrentTime

pad = do
  label <- labelNew (Nothing :: Maybe String)
  widgetShowAll label
  return (toWidget label)
note      = notifyAreaNew defaultNotificationConfig
mpris     = mpris2New
mem       = pollingGraphNew memCfg 1 memCallback
cpu       = pollingGraphNew cpuCfg 0.2 cpuCallback
tray      = systrayNew
bat       = batteryBarNew batCfg 5
mailMain  = mailDirNew "<span fgcolor='yellow'>Main</span>" "/home/igor/.mail/main/INBOX"
mailGmail = mailDirNew "<span fgcolor='yellow'>Gmail</span>" "/home/igor/.mail/gmail/INBOX"
mailUni   = mailDirNew "<span fgcolor='yellow'>Uni</span>" "/home/igor/.mail/uni/INBOX"
mailCern  = mailDirNew "<span fgcolor='yellow'>Cern</span>" "/home/igor/.mail/cern/INBOX"

myPagerConfig :: PagerConfig
myPagerConfig = PagerConfig
  { activeWindow = colorize "white" "" . shorten 60 . escape
  , activeLayout = colorize "grey" ""
  , activeWorkspace = colorize "yellow" "" . escape
  , hiddenWorkspace = colorize "light grey" "" . escape
  , emptyWorkspace = colorize "grey" "" . escape
  , visibleWorkspace = colorize "white" "" . escape
  , urgentWorkspace = colorize "red" "" . escape
  , widgetSep = "   "
  }

pager = taffyPagerNew myPagerConfig

main = defaultTaffybar defaultTaffybarConfig
  { startWidgets = [ pad, pager ]
  , endWidgets = [ pad
                 , myClock
                 , mem
                 , cpu
                 , bat
                 , tray
                 , mailUni
                 , mailGmail
                 , mailMain
                 , mailCern
                 , mpris
                 ]
  , barHeight  = 20
  }
