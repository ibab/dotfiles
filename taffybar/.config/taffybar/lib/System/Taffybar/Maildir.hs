module System.Taffybar.Maildir ( mailDirNew ) where

import System.INotify
import System.Directory
import System.FilePath.Posix
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Set as S
import Text.Regex.Posix

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

import System.Taffybar.Widgets.Util

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = do
  contents <- getDirectoryContents dir
  return $ map (dir </>) contents

isNewMail s | takeFileName s =~ ".*\\.$" = False
            | takeFileName s =~ ",[^,]*S[^,]*$" = False
            | otherwise = True

fillMenu :: MenuClass menu => menu -> FilePath -> IO ()
fillMenu menu path =  do
  paths <- getNewMail path
  contents <- forM paths readFile
  info <- forM contents $ \x -> do
    let from = x =~ "From: (.*)$" !! 0 !! 1
        subject = x =~ "Subject: (.*)$" !! 0 !! 1
    return $ "> "++from ++ "\n    " ++ subject
    --return $ (subject :: String)
  forM_ info $ \x -> do
    item <- menuItemNewWithLabel x
    menuShellAppend menu item
    widgetShow item

emptyMenu :: MenuClass menu => menu -> IO ()
emptyMenu menu = containerForeach menu $ \item ->
                 containerRemove menu item >> widgetDestroy item

getNewMail path = do
  contNew <- getAbsDirectoryContents (path </> "new")
  contCur <- getAbsDirectoryContents (path </> "cur")
  return $ filter isNewMail $ contNew ++ contCur

mailCallback name path label widget _ = do
  newMail <- getNewMail path
  let count = length newMail

  case count of
    0 -> do
      postGUIAsync $ widgetHideAll widget
    _ -> do
      labelSetMarkup label (name ++ ": " ++ show count)
      postGUIAsync $ widgetShowAll widget

initMaildir name path label widget = do
  mailCallback name path label widget ()
  inot <- initINotify
  watchSubDir inot "/new"
  watchSubDir inot "/cur"
  return ()
    where watchSubDir i p = addWatch i [MoveIn, MoveOut, Create, Delete] (path ++ p) (mailCallback name path label widget)

mailDirNew name path = do
  label <- labelNew (Nothing :: Maybe String)
  title <- menuItemNew
  widgetSetName title "title"
  containerAdd title label

  mailMenu <- menuBarNew
  widgetSetName mailMenu "MailMenu"
  containerAdd mailMenu title

  rcParseString $ unlines [ "style 'MailMenu' {"
                          , "  xthickness = 0"
                          , "  GtkMenuBar::internal-padding = 0"
                          , "}"
                          , "style 'title' {"
                          , "  xthickness = 0"
                          , "  GtkMenuItem::horizontal-padding = 0"
                          , "}"
                          , "widget '*MailMenu' style 'MailMenu'"
                          , "widget '*MailMenu*title' style 'title'"
                          ]

  menu <- menuNew
  widgetSetName menu "menu"
  menuTop <- widgetGetToplevel menu
  widgetSetName menuTop "Taffybar_MaildirMenu"
  menuItemSetSubmenu title menu
  _ <- on title menuItemActivate $ fillMenu menu path
  _ <- on title menuItemDeselect $ emptyMenu menu

  on mailMenu realize $ initMaildir name path label mailMenu

  widgetShowAll mailMenu
  --widgetShowAll label
  return $ toWidget mailMenu

