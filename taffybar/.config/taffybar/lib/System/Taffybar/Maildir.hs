module System.Taffybar.Maildir ( mailDirNew ) where

import Text.Regex.Posix
import System.INotify
import System.Directory
import Control.Concurrent

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

isNewMail s | s == "."  = False
            | s == ".." = False
            | s =~ ",[^,]*S[^,]*$" = False
            | otherwise = True

mailCallback name path label _ = do
  contNew <- getDirectoryContents (path ++ "/new")
  contCur <- getDirectoryContents (path ++ "/cur")
  let cont = contNew ++ contCur
      num = length . filter isNewMail $ cont
  case num of
    0 -> do
      postGUIAsync $ widgetHideAll label
    _ -> do
      labelSetMarkup label (name ++ ": " ++ show num)
      postGUIAsync $ widgetShowAll label

initMaildir name path label = do
  mailCallback name path label ()
  inot <- initINotify
  watchSubDir inot "/new"
  watchSubDir inot "/cur"
  return ()
    where watchSubDir i p = addWatch i [MoveIn] (path ++ p) (mailCallback name path label)

mailDirNew name path = do
  label <- labelNew Nothing
  on label realize $ initMaildir name path label
  --box <- hBoxNew False 2
  --image <- imageNewFromFile "/home/igor/test.png"
  --boxPackStart box image PackGrow 0
  --boxPackStart box label PackGrow 0
  --widgetShowAll box
  widgetShowAll label
  return $ toWidget label
