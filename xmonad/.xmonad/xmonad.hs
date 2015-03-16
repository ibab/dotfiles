{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import System.Directory
import System.FilePath.Posix
import Data.Monoid
import qualified Data.Map as M
import Control.Exception
import System.Exit

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Place
import XMonad.Hooks.ICCCMFocus

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Ssh

import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig

import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import Custom.XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MosaicAlt
import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.BinarySpacePartition as BSP

import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders

import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger

import XMonad.Hooks.EwmhDesktops (ewmh)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad.Hooks.SetWMName

editor         = "gvim"
syncClipboard  = "xcmenuctrl -spc > /dev/null"
lockScreen     = "dm-tool lock"

restartXMonad = spawn "xmonad --recompile && xmonad --restart && notify-send 'Restarted XMonad'"

data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete _ c  = c
  nextCompletion      _  = getNextCompletion

passPrompt :: XPConfig -> X ()
passPrompt c = do
  li <- io getPasswords
  mkXPrompt Pass c (mkComplFunFromList li) selectPassword

selectPassword :: String -> X ()
selectPassword s = spawn $ "pass -c " ++ s

getPasswords :: IO [String]
getPasswords = do
  home <- getEnv "HOME"
  entries <- getDirectoryContents $ home ++ "/.password-store"
  return $ map takeBaseName entries

myKeys conf = mkKeymap conf $ [ 
  ("M-<Return>",             spawn (terminal conf)                        ),
  ("M-S-<Return>",           spawn $ "urxvt -cd \"`xcwd`\"; "             ),
  ("M-e",                    spawn "gvim -c \"cd `xcwd`\""                ),
  ("M-<Backspace>",          kill                                         ),
  ("M-<Space>",              sendMessage NextLayout                       ),
--  ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"                ),
--  ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"                ),
--  ("<XF86AudioMute>",        return ()                                    ),
  ("<XF86AudioPlay>",        spawn "mpc toggle"                           ),
  ("<XF86AudioNext>",        spawn "mpc next"                             ),
  ("<XF86AudioPrev>",        spawn "mpc prev"                             ),
  ("M-<F11>",                spawn "mpc stop"                             ),
  ("<XF86MonBrightnessUp>",  spawn "brightness up"                        ),
  ("<XF86MonBrightnessDown>",spawn "brightness down"                      ),
  ("M-j",                    windows W.focusDown                          ),
  ("M-k",                    windows W.focusUp                            ),
  ("M-S-j",                  windows W.swapDown                           ),
  ("M-S-k",                  windows W.swapUp                             ),
  ("M-h",                    sendMessage Shrink >> sendMessage (ExpandTowards L) ),
  ("M-l",                    sendMessage Expand >> sendMessage (ExpandTowards R) ),
  ("M-S-l",                  sendMessage $ ShrinkFrom L                   ),
  ("M-S-h",                  sendMessage $ ShrinkFrom R                   ),
  ("M-b",                    sendMessage ToggleStruts                     ),
  ("M-S-b",                  withFocused toggleBorder                     ),
  ("M-c",                    spawn syncClipboard                          ),
  ("M-S-q",                  io exitSuccess                               ),
  ("M-S-r",                  restartXMonad                                ),
  ("M-x",                    shellPrompt promptConfig                     ),
  ("M-z",                    passPrompt promptConfig                      ),
  ("M-C-l",                  spawn lockScreen                             ),
  ("M-t",                    spawn "toggle.sh"                            ),
  ("M-S-t",                  withFocused $ windows . W.sink               ),
  ("<Print>",                spawn "gnome-screenshot"                     ),
  ("M-<Up>",                 sendMessage $ ExpandTowards U                ),
  ("M-C-<Down>",             sendMessage $ ExpandTowards D                ),
  ("M-<Down>",               sendMessage $ ShrinkFrom U                   ),
  ("M-C-<Up>",               sendMessage $ ShrinkFrom D                   ),
  ("M-s",                    sendMessage $ BSP.Swap                           ),
  ("M-r",                    sendMessage $ Rotate                         ),
  ("<XF86Launch1>",          spawn "gnome-control-center"                 ),
  ("<XF86Display>",          spawn "xtoggle_vert"                         ),
  ("<XF86ScreenSaver>",      spawn lockScreen                             ) ]
  ++ [("M-"   ++ k, windows $ W.greedyView i) | (i, k) <- workspaceMap ]
  ++ [("M-S-" ++ k, windows $ W.shift      i) | (i, k) <- workspaceMap ]
  ++ [("M-"   ++ k, switchTo scr            ) | (k, scr) <- screenMap  ]
  ++ [("M-S-" ++ k, moveTo   scr            ) | (k, scr) <- screenMap  ]
    where workspaceMap  = zip (XMonad.workspaces conf) (map show $ [1..9] ++ [0])
          screenMap     = zip ["a", "s", "d"] [0..]
          switchTo scr  = screenWorkspace scr >>= flip whenJust (windows . W.view)
          moveTo scr    = screenWorkspace scr >>= flip whenJust (windows . W.shift)

myMouseBindings (XConfig { XMonad.modMask = modm }) = M.fromList [
  ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster  ),
  ((modm, button2), \w -> focus w >> windows W.shiftMaster                       ),
  ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) ]

promptConfig = defaultXPConfig
  { font        = "xft:Source Code Pro:pixelsize=12"
  , borderColor = "#1e2320"
  , fgColor     = "#dddddd"
  , fgHLight    = "#ffffff"
  , bgColor     = "#1e2320"
  , bgHLight    = "#5f5f5f"
  , height      = 20
  , position    = Top
  }

myManageHook = manageDocks <> matches <> placeHook (inBounds (underMouse (0.5,0.5))) 
matches = composeAll
  [ title     =? "Eclipse"           --> doCenterFloat
  , title     =? "Eclipse SDK"       --> doCenterFloat
  , title     =? "Vidyo"       --> doFloat
  , title     =? "VidyoDesktop"       --> doFloat
  , title     =? "Welcome to Wolfram Mathematica" --> doFloat
  , isFullscreen                     --> doFullFloat
  , isDialog                         --> doCenterFloat
  , role      =? "pop-up"            --> doCenterFloat
  ]
    where role = stringProperty "WM_WINDOW_ROLE"

myTheme = defaultTheme
  { decoHeight = 16
  , activeColor = "#3f3f3f"
  , activeBorderColor = "#5f5f5f"
  , inactiveColor = "#1e2320"
  , inactiveBorderColor = "#3f3f3f"
  , fontName = "xft:DejaVu Sans Mono:pixelsize=10"
  }

myLayout = modify emptyBSP ||| addTitleBars (modify  emptyBSP)
  where
    modify = manageBorders . avoidStruts
    addTitleBars = noFrillsDeco shrinkText myTheme
    manageBorders = lessBorders OnlyFloat
    tiled  = Tall 1 (1 / 150) (1 / phi)
    tiledSpace = spacing 30 $ tiled
    phi = toRational $ (1 + sqrt 5) / 2

main = do 
  xmonad $ ewmh $ pagerHints $ defaultConfig
    {   terminal           = "urxvt"
    ,   focusFollowsMouse  = True
    ,   borderWidth        = 2
    ,   modMask            = mod4Mask
    ,   workspaces         = ["α", "β", "γ", "δ", "ε"]
    ,   normalBorderColor  = "#3f3f3f"
    ,   focusedBorderColor = "#5f5f5f"--"#990000"
    ,   keys               = myKeys
    ,   mouseBindings      = myMouseBindings
    ,   layoutHook         = myLayout
    ,   manageHook         = myManageHook
    ,   handleEventHook    = fullscreenEventHook <> docksEventHook
    ,   logHook            = mempty <> takeTopFocus
    ,   startupHook        = spawnOnce "taffybar"
    }


