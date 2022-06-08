{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as GDK
import System.Directory
import System.Posix.User
import System.Process
import Data.Char (chr)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.String ( fromString )
import System.IO
import Network.HTTP.Req as Req
import Control.Monad.IO.Class
import Data.HashMap
import qualified Data.ByteString.Lazy as B
import Text.JSON.Generic
import qualified Data.Aeson.Schema as DAS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTPClient


type MySchema = [DAS.schema|
 {
  observations: List
    {
      stationID: Text,
      obsTimeUtc: Text,
      obsTimeLocal: Text,
      neighborhood: Text,
      softwareType: Maybe Text,
      country: Text,
      solarRadiation: Float,
      lon: Float,
      realtimeFrequency: Maybe Int,
      epoch: Int,
      lat: Float,
      uv: Int,
      winddir: Int,
      humidity: Int,
      qcStatus: Int,
      imperial: {
        temp: Int,
        heatIndex: Int,
        dewpt: Int,
        windChill: Int,
        windSpeed: Int,
        windGust: Int,
        pressure: Float,
        precipRate: Int,
        precipTotal: Float,
        elev: Int
      }
    }
}
|]

newtype Observations = Observations
  { observations :: [Observation]
  } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Imperial = Imperial {
  temp :: Integer,
  heatIndex :: Integer,
  dewpt :: Integer,
  windChill :: Integer,
  windSpeed :: Integer,
  windGust :: Integer,
  pressure :: Double,
  precipRate :: Integer,
  precipTotal :: Integer,
  elev :: Integer
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Observation  = Observation {
  stationID :: String,
  obsTimeUtc :: String,
  neighborhood :: String,
  softwareType :: Maybe String,
  country:: String,
  solarRadiation :: Double,
  lon :: Double,
  realtimeFrequency :: Maybe String,
  epoch :: Integer,
  lat :: Double,
  uv :: Integer,
  winddir :: Integer,
  humidity :: Integer,
  qcStatus :: Integer,
  imperial :: Imperial
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

fromJust (Just x) = x
fromJust Nothing = error "Maybe.fromJust: Nothing"

getWeather :: IO (JsonResponse Value)
getWeather =
  runReq defaultHttpConfig $
  req GET (https "api.weather.com" /: "v2" /: "pws" /: "observations" /: "current") NoReqBody jsonResponse $
  "apiKey" =: ("e1f10a1e78da46f5b10a1e78da96f525" :: String) <>
  "units" =: ("e" :: String) <>
  "stationId" =: ("KMNCOONR65" :: String) <>
  "format" =: ("json" :: String)

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

getObservation :: IO Observation
getObservation = do
  payload <- getWeather
  let response = (responseBody  payload)

  let justObservations = fromJSONValue response :: Maybe Observations
  let observationList = (fromJust (justObservations))
  let list = (observations observationList)
  let observation = (head list)
  let imperialData = (imperial observation)
  print (imperialData)
  return observation

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
  Gtk.labelSetMarkup label2 ("<b>Weather" <> "</b>")

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
    -- getObservation
    print "test"

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

  #attach grid btn1   0 0 1 1
  #attach grid label1 0 1 1 1
  #attach grid btn2   1 0 1 1
  #attach grid label2 1 1 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  -- obj <- either fail return =<<
  --   eitherDecodeFileStrict "example.json" :: IO (DAS.Object MySchema)
  -- print [DAS.get| obj.observations[].stationID |]
  observation <- getObservation
  let imperialData = (imperial observation)
  print observation
  let temperature = (temp imperialData)
  Gtk.labelSetMarkup label2 ("<b>" <> T.pack (show temperature) <> " fahrenheit</b>")
  Gtk.main
