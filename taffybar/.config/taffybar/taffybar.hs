import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS2
import System.Taffybar.Battery
import System.Taffybar.Maildir

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

batColor :: Double -> (Double, Double, Double)
batColor p = if p < 0.99 then (r, g, 0) else (0, 0, 1)
  where r = if p < 0.5 then 1.0 else 1.0 - (p * 100 - 50) * 5.12/256.0
        g = if p > 0.5 then 1.0 else p * 100 * 5.12/256.0

memCfg = defaultGraphConfig { graphDataColors = [(22/255, 147/255, 165/255, 1)]
                            , graphLabel = Nothing
                            , graphWidth = 40
                            , graphPadding = 1
                            , graphBorderColor = (0.247, 0.247, 0.247)
                            }
cpuCfg = defaultGraphConfig { graphDataColors = [ (251/255, 184/255, 41/255, 1)
                                                , (1, 0, 1, 0.5)
                                                ]
                            , graphLabel = Nothing
                            , graphWidth = 40
                            , graphPadding = 1
                            , graphBorderColor = (0.247, 0.247, 0.247)
                            }
batCfg = defaultBatteryConfig { barPadding     = 1
                              , barColor       = batColor
                              , barBorderColor = (0.247, 0.247, 0.247)
                              }
clock = textClockNew Nothing "%a %Y-%m-%d <span fgcolor='yellow'>%H:%M </span><span fgcolor='red'>•</span> " 1
pad = do
  label <- labelNew Nothing
  widgetShowAll label
  return (toWidget label)
logger    = xmonadLogNew
note      = notifyAreaNew defaultNotificationConfig
mpris     = mpris2New
mem       = pollingGraphNew memCfg 4 memCallback
cpu       = pollingGraphNew cpuCfg 0.3 cpuCallback
tray      = systrayNew
bat       = batteryBarNew batCfg 5
mailMain  = mailDirNew "<span fgcolor='yellow'>M</span>" "/home/igor/.mail/main/INBOX"
mailGmail = mailDirNew "<span fgcolor='yellow'>G</span>" "/home/igor/.mail/gmail/INBOX"
mailUni   = mailDirNew "<span fgcolor='yellow'>U</span>" "/home/igor/.mail/uni/INBOX"

main = defaultTaffybar defaultTaffybarConfig {
  startWidgets = [ pad
                 , logger
               --, note
                 ],
  endWidgets = [ clock
               ,  mem
               ,  cpu
               ,  bat
               ,  tray
               ,  mailUni
               ,  mailGmail
               ,  mailMain
               ,  mpris
               ],
  barHeight  = 20
}
