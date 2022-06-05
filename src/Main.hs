{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as GDK
import System.Directory
import System.Posix.User
import System.Process
import Data.Text as Text
import Data.Char (chr)

main :: IO ()
main = do
  Gtk.init Nothing

  home <- getHomeDirectory
  user <- getEffectiveUserName

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth win 10
  Gtk.setWindowTitle win "ByeBye"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 750
  Gtk.setWindowDefaultHeight win 225
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter
  Gtk.windowSetDecorated win False

  img1 <- Gtk.imageNewFromFile $ home ++ "/.local/img/cancel.png"
  img2 <- Gtk.imageNewFromFile $ home ++ "/.local/img/logout.png"
  img3 <- Gtk.imageNewFromFile $ home ++ "/.local/img/reboot.png"
  img4 <- Gtk.imageNewFromFile $ home ++ "/.local/img/shutdown.png"
  img5 <- Gtk.imageNewFromFile $ home ++ "/.local/img/suspend.png"
  img6 <- Gtk.imageNewFromFile $ home ++ "/.local/img/hibernate.png"
  img7 <- Gtk.imageNewFromFile $ home ++ "/.local/img/lock.png"


  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 "<b>Cancel</b>"

  label2 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label2 "<b>Logout</b>"

  label3 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label3 "<b>Reboot</b>"

  label4 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label4 "<b>Shutdown</b>"

  label5 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label5 "<b>Suspend</b>"

  label6 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label6 "<b>Hibernate</b>"

  label7 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label7 "<b>Lock</b>"

  btn1 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn1 $ Just img1
  Gtk.widgetSetHexpand btn1 False
  on btn1 #clicked $ do
    putStrLn "User choose: Cancel"
    Gtk.widgetDestroy win

  btn2 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn2 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn2 $ Just img2
  Gtk.widgetSetHexpand btn2 False
  on btn2 #clicked $ do
    putStrLn "User choose: Logout"
    callCommand $ "pkill -u " ++ user
    -- callCommand "killall xmonad-x86_64-linux"
    -- callCommand "pkill -v xmonad"
    -- callCommand "pkill -f xmonad-start"

  btn3 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn3 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn3 $ Just img3
  Gtk.widgetSetHexpand btn3 False
  on btn3 #clicked $ do
    putStrLn "User choose: Reboot"
    -- callCommand "reboot"
    callCommand "sudo shutdown -r now"

  btn4 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn4 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn4 $ Just img4
  Gtk.widgetSetHexpand btn4 False
  on btn4 #clicked $ do
    putStrLn "User choose: Shutdown"
    callCommand "sudo shutdown -h now"

  btn5 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn5 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn5 $ Just img5
  Gtk.widgetSetHexpand btn5 False
  on btn5 #clicked $ do
    putStrLn "User choose: Suspend"
    callCommand "systemctl suspend"

  btn6 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn6 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn6 $ Just img6
  Gtk.widgetSetHexpand btn6 False
  on btn6 #clicked $ do
    putStrLn "User choose: Hibernate"
    callCommand "systemctl hibernate"

  btn7 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn7 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn7 $ Just img7
  Gtk.widgetSetHexpand btn7 False
  on btn7 #clicked $ do
    putStrLn "User choose: Lock"
    -- callCommand "slock"
    callCommand "i3lock -d -c FFFFFF -i ~/.local/wallpaper/mountain-road.png"
    Gtk.mainQuit

  on win #keyPressEvent $ \keyEvent -> do
    key <- keyEvent `get` #keyval >>= GDK.keyvalToUnicode
    -- putStrLn $ "Key pressed: ‘" ++ (chr (fromIntegral key) : []) ++ "’ (" ++ show key ++ ")"
    putStrLn $ "Key pressed: (" ++ show key ++ ")"
    if key == 27 then Gtk.mainQuit else pure ()
    return False

  grid <- Gtk.gridNew
  Gtk.gridSetColumnSpacing grid 10
  Gtk.gridSetRowSpacing grid 10
  Gtk.gridSetColumnHomogeneous grid True

  -- #keyPressEvent \(EventKey k) -> True <$ do
  --   kv <- foo $ managedForeignPtr k
  --   putStrLn $ "key pressed: 0x" ++ showHex kv ""
  --   bool (return ()) G.mainQuit $ kv == 0x71

  #attach grid btn1   0 0 1 1
  #attach grid label1 0 1 1 1
  #attach grid btn2   1 0 1 1
  #attach grid label2 1 1 1 1
  #attach grid btn3   2 0 1 1
  #attach grid label3 2 1 1 1
  #attach grid btn4   3 0 1 1
  #attach grid label4 3 1 1 1
  -- #attach grid btn5   4 0 1 1
  -- #attach grid label5 4 1 1 1
  -- #attach grid btn6   5 0 1 1
  -- #attach grid label6 5 1 1 1
  #attach grid btn7   6 0 1 1
  #attach grid label7 6 1 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win
  Gtk.main
