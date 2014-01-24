module System.Taffybar.Maildir ( mailDirNew ) where

import System.INotify
import System.Directory
import System.FilePath.Posix
import Data.List
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Set as S
import Text.Regex.Posix

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Image
import Graphics.UI.Gtk.Layout.HBox

import System.Taffybar.Widgets.Util

makeMailList :: IO Window
makeMailList = do
  container <- windowNew
  return container

toggleMailList :: WidgetClass w => w -> Window -> IO Bool
toggleMailList w c = do
  visible <- get c widgetVisible
  if visible then widgetHideAll c
             else do
               attachPopup w "Mails" c
               displayPopup w c
  return True

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = do
  contents <- getDirectoryContents dir
  return $ map (dir </>) contents

isNewMail s | takeFileName s =~ ".*\\.$" = False
            | takeFileName s =~ ",[^,]*S[^,]*$" = False
            | otherwise = True

createTooltipText newMail = do
  conts <- forM newMail readFile
  info <- forM conts $ \x -> do
      let from = x =~ "From: (.*)$" !! 0 !! 1
          subject = x =~ "Subject: (.*)$" !! 0 !! 1
      return $ from ++ ": " ++ subject
  return $ unlines info

mailCallback name path label tips _ = do
  contNew <- getAbsDirectoryContents (path </> "new")
  contCur <- getAbsDirectoryContents (path </> "cur")
  let newMail = filter isNewMail $ contNew ++ contCur
  let count = length newMail

  case count of
    0 -> do
      postGUIAsync $ widgetHideAll label
    _ -> do
      text <- createTooltipText newMail
      --tooltipsSetTip tips label text text
      labelSetMarkup label (name ++ ": " ++ show count)
      postGUIAsync $ widgetShowAll label

initMaildir name path label tips = do
  mailCallback name path label tips ()
  inot <- initINotify
  watchSubDir inot "/new"
  watchSubDir inot "/cur"
  return ()
    where watchSubDir i p = addWatch i [MoveIn] (path ++ p) (mailCallback name path label tips)

mailDirNew name path = do
  label <- labelNew Nothing
  tips <- tooltipsNew
  on label realize $ initMaildir name path label tips
  --box <- hBoxNew False 2
  --image <- imageNewFromFile "/home/igor/test.png"
  --boxPackStart box image PackGrow 0
  --boxPackStart box label PackGrow 0
  --widgetShowAll box
  widgetShowAll label
  return $ toWidget label


