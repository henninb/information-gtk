{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as GDK
import System.Directory
import System.Posix.User
import System.Process
import Data.Text as Text
import Data.Char (chr)
import Data.Aeson
import GHC.Generics


data Category = Category
    {category :: String,
    categoryId  :: Integer
    } deriving (Show, Generic, Eq, ToJSON, FromJSON)

-- instance FromRow Category
-- instance ToRow Category

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

  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 "<b>Cancel</b>"

  label2 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label2 "<b>Weather</b>"

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
    putStrLn "User choose: weather"
    callCommand $ "weather-minneapolis"


    -- Gtk.mainQuit

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
  -- #attach grid btn3   2 0 1 1
  -- #attach grid label3 2 1 1 1
  -- #attach grid btn4   3 0 1 1
  -- #attach grid label4 3 1 1 1
  -- -- #attach grid btn5   4 0 1 1
  -- -- #attach grid label5 4 1 1 1
  -- -- #attach grid btn6   5 0 1 1
  -- -- #attach grid label6 5 1 1 1
  -- #attach grid btn7   6 0 1 1
  -- #attach grid label7 6 1 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win
  Gtk.main
