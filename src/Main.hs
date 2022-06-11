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
import System.Directory (getHomeDirectory)
import System.Posix.User (getEffectiveUserName)
import Data.Char (chr)
import Data.Aeson (eitherDecode, encode, eitherDecodeStrict)
import Data.Aeson.Types (ToJSON, FromJSON, Value, parseJSON, parseMaybe)
import GHC.Generics (Generic)
import Data.String ( fromString )
-- import Network.HTTP.Req (JsonResponse, jsonResponse, responseBody, (/:), defaultHttpConfig, (=:), https, runReq, req, NoReqBody, GET)
import Network.HTTP.Req
-- import Prelude
-- import Control.Monad.IO.Class
-- import Data.HashMap

-- import Data.Text.Lazy             as TL
-- import Data.Text.Lazy.Encoding    as TL
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
import Data.Text (pack)
-- import Data.Text.Encoding
import Text.JSON.Generic (Typeable)
-- import qualified Data.Aeson.Schema as DAS
import Data.Aeson.Schema (schema, Object, get)
-- import qualified Network.HTTP.Client as HTTPClient
-- type TestSchema = [schema| #List | { id: Text }} |]

type WeatherSchema = [schema|
  {
    id: Text,
    v3-wx-observations-current: {
      cloudCeiling: Maybe Text,
      cloudCoverPhrase: Text,
      dayOfWeek: Text,
      dayOrNight: Text,
      expirationTimeUtc: Int,
      iconCode: Int,
      iconCodeExtend: Int,
      obsQualifierCode: Text,
      obsQualifierSeverity: Int,
      precip1Hour: Float,
      precip6Hour: Float,
      precip24Hour: Float,
      pressureAltimeter: Float,
      pressureChange: Float,
      pressureMeanSeaLevel: Float,
      pressureTendencyCode: Int,
      pressureTendencyTrend: Text,
      relativeHumidity: Int,
      snow1Hour: Int,
      snow6Hour: Int,
      snow24Hour: Int,
      sunriseTimeLocal: Text,
      sunriseTimeUtc: Int,
      sunsetTimeLocal: Text,
      sunsetTimeUtc: Int,
      temperature: Int,
      temperatureChange24Hour: Int,
      temperatureDewPoint: Int,
      temperatureFeelsLike: Int,
      temperatureHeatIndex: Int,
      temperatureMax24Hour: Int,
      temperatureMaxSince7Am: Int,
      temperatureMin24Hour: Int,
      temperatureWindChill: Int,
      uvDescription: Text,
      uvIndex: Int,
      validTimeLocal: Text,
      validTimeUtc: Int,
      visibility: Int,
      windDirection: Int,
      windDirectionCardinal: Text,
      windGust: Maybe Int,
      windSpeed: Int,
      wxPhraseLong: Text,
      wxPhraseMedium: Text,
      wxPhraseShort: Text
    }
  }
|]

type MySchema = [schema|
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
      realtimeFrequency: Maybe Float,
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
        precipRate: Float,
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
  precipRate :: Double,
  precipTotal :: Double,
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

data Weather = Weather {
    id:: String,
    -- v3-wx-observations-current:: ObservationV3
     observation3:: ObservationV3
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data ObservationV3 = ObservationV3 {
      cloudCeiling:: Maybe String,
      cloudCoverPhrase:: String,
      dayOfWeek:: String,
      dayOrNight:: String,
      expirationTimeUtc:: Integer,
      iconCode:: Integer,
      iconCodeExtend:: Integer,
      obsQualifierCode:: String,
      obsQualifierSeverity:: Int,
      precip1Hour:: Float,
      precip6Hour:: Float,
      precip24Hour:: Float,
      pressureAltimeter:: Float,
      pressureChange:: Float,
      pressureMeanSeaLevel:: Float,
      pressureTendencyCode:: Int,
      pressureTendencyTrend:: String,
      relativeHumidity:: Int,
      snow1Hour:: Int,
      snow6Hour:: Int,
      snow24Hour:: Int,
      sunriseTimeLocal:: String,
      sunriseTimeUtc:: Int,
      sunsetTimeLocal:: String,
      sunsetTimeUtc:: Int,
      temperature:: Int,
      temperatureChange24Hour:: Int,
      temperatureDewPoint:: Int,
      temperatureFeelsLike:: Int,
      temperatureHeatIndex:: Int,
      temperatureMax24Hour:: Int,
      temperatureMaxSince7Am:: Int,
      temperatureMin24Hour:: Int,
      temperatureWindChill:: Int,
      uvDescription:: String,
      uvIndex:: Int,
      validTimeLocal:: String,
      validTimeUtc:: Int,
      visibility:: Int,
      windDirection:: Int,
      windDirectionCardinal:: String,
      -- windGust:: Maybe Int,
      -- windSpeed:: Int,
      wxPhraseLong:: String,
      wxPhraseMedium:: String,
      wxPhraseShort:: String
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

fromJust (Just x) = x
fromJust Nothing = error "Maybe.fromJust: Nothing"

weatherApi :: IO (JsonResponse Value)
weatherApi =
  runReq defaultHttpConfig $
  req GET (https "api.weather.com" /: "v3" /: "aggcommon" /: "v3-wx-observations-current") NoReqBody jsonResponse $
  "apiKey" =: ("e1f10a1e78da46f5b10a1e78da96f525" :: String) <>
  "geocodes" =: ("45.18,-93.32" :: String) <>
  "units" =: ("e" :: String) <>
  "language" =: ("en-US" :: String) <>
  "format" =: ("json" :: String)

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
  let response = (responseBody payload)

  let justObservations = fromJSONValue response :: Maybe Observations
  let observationList = (fromJust (justObservations))
  let list = (observations observationList)
  let observation = (head list)
  let imperialData = (imperial observation)
  return observation

getObservationPayload :: IO BL.ByteString
getObservationPayload = do
  payload <- getWeather
  let response = (responseBody payload)
  return (Data.Aeson.encode response)

getWeatherApiPayload :: IO BL.ByteString
getWeatherApiPayload = do
  payload <- weatherApi
  let response = (responseBody payload)
  return (Data.Aeson.encode response)

testme = do
  payload <- getObservationPayload -- (BL.ByteString)
  output <- either fail return $ eitherDecode payload :: IO (Object MySchema)
  print payload
  print "-----"
  print output
  print [Data.Aeson.Schema.get| output.observations[].stationID |]
  return ([Data.Aeson.Schema.get| output.observations[].imperial.temp |])
  -- print Right (zz)

testmeToo = do
  payload <- getWeatherApiPayload -- (BL.ByteString)
  output <- either fail return $ eitherDecode payload :: IO (Object WeatherSchema)
  print payload
  print "-----"
  print output
  -- print [Data.Aeson.Schema.get| output.observations[].stationID |]
  -- return ([Data.Aeson.Schema.get| output.observations[].imperial.temp |])

main :: IO ()
main = do
  Gtk.init Nothing

  home <- getHomeDirectory
  user <- getEffectiveUserName

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth win 10
  Gtk.setWindowTitle win "Weather"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 750
  Gtk.setWindowDefaultHeight win 225
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter
  Gtk.windowSetDecorated win False

  img1 <- Gtk.imageNewFromFile $ home ++ "/.local/img/cancel.png"
  img2 <- Gtk.imageNewFromFile $ home ++ "/.local/img/logout.png"

  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 ("<b>" <> "Done" <> "</b>")

  label2 <- Gtk.labelNew Nothing
  -- Gtk.widgetOverrideFontSource label2
  -- Gtk.lebelSetFont label2

-- $title->modify_font(new PangoFontDescription("Times New Roman Italic 10"));

  -- label3 <- Gtk.labelNew Nothing
  -- Gtk.labelSetMarkup label3 ("<b>" <> "Humidity: 50" <> "</b>")

  label4 <- Gtk.labelNew Nothing
  label5 <- Gtk.labelNew Nothing
  label6 <- Gtk.labelNew Nothing
  label7 <- Gtk.labelNew Nothing
  label8 <- Gtk.labelNew Nothing
  label9 <- Gtk.labelNew Nothing
  label10 <- Gtk.labelNew Nothing
  label11 <- Gtk.labelNew Nothing

  btn1 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn1 $ Just img1
  Gtk.widgetSetHexpand btn1 False
  on btn1 #clicked $ do
    putStrLn "User chose: Done"
    Gtk.widgetDestroy win

  btn2 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn2 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn2 $ Just img2
  Gtk.widgetSetHexpand btn2 False
  on btn2 #clicked $ do
    print "test"

  on win #keyPressEvent $ \keyEvent -> do
    key <- keyEvent `Data.GI.Base.get` #keyval >>= GDK.keyvalToUnicode
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
  -- #attach grid btn2   1 0 1 1
  #attach grid label2 1 1 1 1
  -- #attach grid label3 1 2 1 1
  #attach grid label4 1 2 1 1
  #attach grid label5 1 3 1 1
  #attach grid label6 1 4 1 1
  #attach grid label7 1 5 1 1
  #attach grid label8 1 6 1 1
  #attach grid label9 1 7 1 1
  #attach grid label10 1 8 1 1
  #attach grid label11 1 9 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  obj <- getObservationPayload
  -- let x = eitherDecodeStrict obj :: IO (DAS.Object MySchema)
  print obj

  -- o <- either fail return $ eitherDecode "{ \"foo\": { \"bar\": 1 } }" :: IO (DAS.Object MySchema)

  -- obj <- either fail return =<<
  --   eitherDecodeFileStrict "example.json" :: IO (DAS.Object MySchema)
  -- print [DAS.get| obj.observations[].stationID |]
  observation <- getObservation
  let imperialData = (imperial observation)
  -- let temperature = (temp imperialData)
  Gtk.labelSetMarkup label2 ("<b>" <> "Temperature: " <> pack (show (temp imperialData)) <> " F" <> "</b>")
  Gtk.labelSetMarkup label4 ("<b>" <> "Pressure: " <> pack (show (pressure imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label5 ("<b>" <> "Windchill: " <> pack (show (windChill imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label6 ("<b>" <> "Wind Gust: " <> pack (show (windGust imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label7 ("<b>" <> "Wind Speed: " <> pack (show (windSpeed imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label8 ("<b>" <> "Heat Index: " <> pack (show (heatIndex imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label9 ("<b>" <> "Dew Point: " <> pack (show (dewpt imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label10 ("<b>" <> "Precipitation Rate: " <> pack (show (precipRate imperialData)) <> "" <> "</b>")
  Gtk.labelSetMarkup label11 ("<b>" <> "Precipitation: " <> pack (show (precipTotal imperialData)) <> "" <> "</b>")
  Gtk.main
