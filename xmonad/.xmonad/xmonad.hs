{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid
import qualified Data.Map as M
import Control.Exception
import System.Exit

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig

import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed

import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders

import System.Taffybar.XMonadLog
import DBus.Client


editor         = "gvim"
syncClipboard  = "lolictrl -spc > /dev/null"
lockScreen     = "i3lock -i /usr/local/wallpapers/background"
commonApps     = [ "firefox"
                 , "nautilus"
                 , "vlc"
                 ]

restartXMonad = spawn "xmonad --recompile && xmonad --restart && notify-send 'Restarted XMonad'"

myKeys conf = mkKeymap conf $ [ 
  ("M-<Return>",             spawn (terminal conf)                        ),
  ("M-e",                    spawn editor                                 ),
  ("M-<Backspace>",          kill                                         ),
  ("M-<Space>",              sendMessage NextLayout                       ),
--  ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"                ),
--  ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"                ),
--  ("<XF86AudioMute>",        return ()                                    ),
  ("<XF86AudioPlay>",        spawn "mpc toggle"                           ),
  ("<XF86AudioNext>",        spawn "mpc next"                             ),
  ("<XF86AudioPrev>",        spawn "mpc prev"                             ),
  ("M-<F11>",                spawn "mpc stop"                             ),
  ("M-j",                    windows W.focusDown                          ),
  ("M-k",                    windows W.focusUp                            ),
  ("M-S-j",                  windows W.swapDown                           ),
  ("M-S-k",                  windows W.swapUp                             ),
  ("M-h",                    sendMessage Shrink                           ),
  ("M-l",                    sendMessage Expand                           ),
  ("M-b",                    sendMessage ToggleStruts                     ),
  ("M-S-b",                  withFocused toggleBorder                     ),
  ("M-c",                    spawn syncClipboard                          ),
  ("M-S-q",                  io exitSuccess                               ),
  ("M-r",                    restartXMonad                                ),
  ("M-x",                    shellPrompt launcherConfig                   ),
  ("M-S-l",                  spawn lockScreen                             ),
  ("M-n",                    spawnSelected defaultGSConfig commonApps     ),
  ("M-t",                    spawn "toggle.sh"                            ),
  ("M-p",                    spawn "myproxy start"                        ),
  ("M-S-p",                  spawn "myproxy stop"                         ),
  ("M-S-t",                  withFocused $ windows . W.sink               ),
  ("<Print>",                spawn "gnome-screenshot"                     ),
  ("<XF86Display>",          spawn "xtoggle_vert"                         ),
  ("<XF86ScreenSaver>",      spawn lockScreen                             ) ]
  ++ [("M-"   ++ k, windows $ W.greedyView i) | (i, k) <- workspaceMap ]
  ++ [("M-S-" ++ k, windows $ W.shift      i) | (i, k) <- workspaceMap ]
  ++ [("M-"   ++ k, switchTo scr            ) | (k, scr) <- screenMap  ]
  ++ [("M-S-" ++ k, moveTo   scr            ) | (k, scr) <- screenMap  ]
    where workspaceMap  = zip (XMonad.workspaces conf) (map show $ [1..9] ++ [0])
          screenMap     = zip ["s", "a", "d"] [0..]
          switchTo scr  = screenWorkspace scr >>= flip whenJust (windows . W.view)
          moveTo scr    = screenWorkspace scr >>= flip whenJust (windows . W.shift)

myMouseBindings (XConfig { XMonad.modMask = modm }) = M.fromList [
  ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster  ),
  ((modm, button2), \w -> focus w >> windows W.shiftMaster                       ),
  ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) ]

launcherConfig = defaultXPConfig
  { font        = "xft:Source Code Pro:pixelsize=12"
  , borderColor = "#3f3f3f"
  , fgColor     = "#dddddd"
  , fgHLight    = "#ffffff"
  , bgColor     = "#3f3f3f"
  , bgHLight    = "#6f6f6f"
  , height      = 18
  , position    = Bottom
  }

myManageHook = manageDocks <+> composeAll
  [ resource  =? "desktop_window"    --> doIgnore
  , resource  =? "kdesktop"          --> doIgnore
  , title     =? "synapse"           --> doIgnore
  , title     =? "Eclipse"           --> doFloat
  , title     =? "Eclipse SDK"       --> doFloat
  , title     =? "Zenity"            --> doFloat
  , title     =? "Gnome-Pie"         --> doIgnore
  , title     =? "devmon unmount"    --> doFloat
  , title     =? "Welcome to Wolfram Mathematica 9" --> doFloat
  , title     =? "Volume Control"    --> doCenterFloat
  , isFullscreen                     --> doFullFloat
  , role      =? "pop-up"            --> doCenterFloat
  ]
    where role = stringProperty "WM_WINDOW_ROLE"

taffyPP = defaultPP
  {   ppCurrent         = wrap "<b>" "</b>"
  ,   ppVisible         = taffybarColor "#fcfcfc" ""
  ,   ppHidden          = taffybarColor "#d0d0d0" ""
  ,   ppHiddenNoWindows = const []
  ,   ppUrgent          = wrap "<b>" "</b>"
  ,   ppTitle           = wrap "<b>" "</b>" . taffybarEscape . cutoff 70
  ,   ppSep             = " <b>::</b> "
  ,   ppWsSep           = " "
  ,   ppLayout          = id
  }
  where wrap x y z = x ++ z ++ y
        cutoff n xs | length xs <= n = xs | otherwise = take n xs ++ "…"

myLayout = (borders . avoidStruts) (tiled ||| Mirror tiled ||| tiledSpace)
  where
    borders = lessBorders OnlyFloat
    tiled  = renamed [Replace "Tiled"] $ Tall 1 (1/100) (1/2)
    tiledSpace = spacing 30 $ tiled

main = do 
  session <- try connectSession

  myLogHook <- case session of
    Right c ->
      return $ dbusLogWithPP c taffyPP
    Left (err::ClientError) -> do
      spawn "notify-send 'Connection to DBus failed'"
      return mempty

  xmonad defaultConfig 
    {   terminal           = "urxvt"
    ,   focusFollowsMouse  = True
    ,   borderWidth        = 2
    ,   modMask            = mod4Mask
    ,   workspaces         = ["α", "β", "γ", "δ", "ε", "ζ"]
    ,   normalBorderColor  = "#3f3f3f"
    ,   focusedBorderColor = "#5f5f5f"--"#990000"
    ,   keys               = myKeys
    ,   mouseBindings      = myMouseBindings
    ,   layoutHook         = myLayout
    ,   manageHook         = myManageHook
    ,   handleEventHook    = fullscreenEventHook
                               <+> docksEventHook 
    ,   logHook            = myLogHook
                               >> setWMName "LG3D"
    ,   startupHook        = return () --spawnOnce "taffybar"
    }
