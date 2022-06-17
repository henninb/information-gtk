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
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.Lazy as BL

import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text (pack, unpack)
import Text.JSON.Generic (Typeable)
-- import qualified Data.Aeson.Schema as DAS
import Data.Aeson.Schema (schema, Object, get)
import Data.Aeson.Casing.Internal (snakeCase)
import Data.Typeable (typeOf)

import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Time (getCurrentTime)
import Data.Time.Format

import qualified GI.Pango
-- import qualified Graphics.Rendering.Pango.Enums as GRPF
-- import qualified Graphics.Rendering.Pango.Font as GRPF
-- import qualified Graphics.Rendering.Pango.Cairo as GRPC
-- import qualified Graphics.Rendering.Pango.Layout as GRPL

type ForecastSchema = [schema|
  {
  '': List   {
      language: Text
    }
  }
|]

type AstroSchema = [schema|
  {
  metadata: {
    language: Text
  }
  ,
  astroData: List {
     dateLocal: Text,
     visibleLight: {
       hours: Int,
       minutes: Int,
       seconds: Int
     },
     lengthOfDay: {
       hours: Int,
       minutes: Int,
       seconds: Int
     },
     tomorrowDaylightDifference: {
       sign: Text,
       minutes: Int,
       seconds: Int
     },
     sun: {
       riseSet: {
         riseLocal: Text,
         riseUTC: Text,
         setLocal: Text,
         setUTC: Text
       }
     },
     moon: {
       riseSet: {
         riseLocal: Text,
         riseUTC: Text,
         setLocal: Text,
         setUTC: Text,
         risePhrase: Text,
         setPhrase: Text,
         moonage: Float,
         percentIlluminated: Int
       }
     }
  },
  astroPhases: List {
    date: Text,
    moonPhase: Text,
    moonAge: Float,
    moonAgeFromPhase: Int
  }

  }
|]

type WeatherSchema = [schema|
  {
  values: List
  {
    id: Text,
    currentObservation: {
      cloudCeiling: Maybe Float,
      cloudCoverPhrase: Text,
      dayOfWeek: Text,
      dayOrNight: Text,
      expirationTimeUtc: Int,
      iconCode: Int,
      iconCodeExtend: Int,
      obsQualifierCode: Maybe Text,
      obsQualifierSeverity: Maybe Int,
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
  }
|]

type ObservationSchema = [schema|
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

-- newtype Observations = Observations
--   { observations :: [Observation]
--   } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

-- data Imperial = Imperial {
--   temp :: Integer,
--   heatIndex :: Integer,
--   dewpt :: Integer,
--   windChill :: Integer,
--   windSpeed :: Integer,
--   windGust :: Integer,
--   pressure :: Double,
--   precipRate :: Double,
--   precipTotal :: Double,
--   elev :: Integer
-- } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

-- data Observation  = Observation {
--   stationID :: String,
--   obsTimeUtc :: String,
--   neighborhood :: String,
--   softwareType :: Maybe String,
--   country:: String,
--   solarRadiation :: Double,
--   lon :: Double,
--   realtimeFrequency :: Maybe String,
--   epoch :: Integer,
--   lat :: Double,
--   uv :: Integer,
--   winddir :: Integer,
--   humidity :: Integer,
--   qcStatus :: Integer,
--   imperial :: Imperial
-- } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Weather = Weather {
    id:: String,
     observation3:: V3WxObservationsCurrent
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data V3WxObservationsCurrent = V3WxObservationsCurrent {
      cloudCeiling:: Maybe Double,
      cloudCoverPhrase:: String,
      dayOfWeek:: String,
      dayOrNight:: String,
      expirationTimeUtc:: Integer,
      iconCode:: Integer,
      iconCodeExtend:: Integer,
      obsQualifierCode:: Maybe String,
      obsQualifierSeverity:: Maybe Int,
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
      windGust:: Maybe Int,
      windSpeed:: Int,
      wxPhraseLong:: String,
      wxPhraseMedium:: String,
      wxPhraseShort:: String
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"

fromIntJust :: Maybe Int -> Int
fromIntJust (Just x) = x
fromIntJust Nothing = 0

apiKey :: String
apiKey = "e1f10a1e78da46f5b10a1e78da96f525"

forecastApi :: IO Value
forecastApi = do
  response <- runReq defaultHttpConfig $ req
    GET (https "api.weather.com" /: "v3" /: "wx" /: "forecast" /: "daily" /: "10day") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "geocode" =: ("45.18,-93.32" :: String) <>
    "units" =: ("e" :: String) <>
    "language" =: ("en-US" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

astroApi :: IO Value
astroApi = do
    now <- getCurrentTime
    response <- runReq defaultHttpConfig $ req
      GET
      (https "api.weather.com" /: "v2" /: "astro") NoReqBody jsonResponse $
      "apiKey" =: (apiKey :: String) <>
      "geocode" =: ("45.18,-93.32" :: String) <>
      "days" =: ("1" :: String) <>
      "date" =: (formatTime defaultTimeLocale "%Y%m%d" now :: String) <>
      "format" =: ("json" :: String)
    -- print (responseBody response :: Value)
    return (responseBody response)

weatherApi :: IO Value
weatherApi = do
  response <- runReq defaultHttpConfig $ req
     GET (https "api.weather.com" /: "v3" /: "aggcommon" /: "v3-wx-observations-current") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "geocodes" =: ("45.18,-93.32" :: String) <>
    "units" =: ("e" :: String) <>
    "language" =: ("en-US" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

getWeather :: IO Value
getWeather = do
  response <- runReq defaultHttpConfig $ req
    GET (https "api.weather.com" /: "v2" /: "pws" /: "observations" /: "current") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "units" =: ("e" :: String) <>
    "stationId" =: ("KMNCOONR65" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- TODO: modify
-- getObservation :: IO Observation
-- getObservation = do
--   payload <- getWeather
--   print . typeOf $ payload
--   let justObservations = fromJSONValue payload :: Maybe Observations
--   let observationList = (fromJust (justObservations))
--   let list = (observations observationList)
--   let observation = (head list)
--   let imperialData = (imperial observation)
--   return observation

getApiWeather :: IO Weather
getApiWeather = do
  payload <- weatherApi
  let justObservations = fromJSONValue payload :: Maybe Weather
  let observationList = (fromJust (justObservations))
  return (observationList)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

getAstroObservation :: IO (Object AstroSchema)
getAstroObservation = do
  payload <- astroApi
  let myPayload = Data.Aeson.encode payload
  output <- either fail return $ eitherDecode myPayload :: IO (Object AstroSchema)
  return (output)

getWeatherObservation :: IO (Object WeatherSchema)
getWeatherObservation = do
  payload <- weatherApi
  let myPayload = Data.Aeson.encode payload -- (BL.ByteString)
  let payloadUpdated = BLU.toString myPayload
  let payloadFinal = replace payloadUpdated "v3-wx-observations-current" "currentObservation"
  let payloadx = BLU.fromString  ("{\"values\": " ++ payloadFinal ++ "}")
  output <- either fail return $ eitherDecode payloadx :: IO (Object WeatherSchema)
  return (output)

-- styles :: Data.ByteString.Internal.ByteString
styles = mconcat
    [ "button { font-size: large; margin: 2pt 8pt; }"
    , "textview { font-size: 25px; }"
    ]

-- printHello :: Gtk.TextView -> Gtk.Label -> IO ()
-- printHello textView label =
--     do
--         buffer <- get textView #buffer
--         text <- get buffer #text

--         case text of
--             Just a ->
--                 do
--                     Gtk.labelSetText label a
--             Nothing -> putStrLn "ss"

textViewGetValue tv = do
    buf <- Gtk.textViewGetBuffer tv
    start <- Gtk.textBufferGetStartIter buf
    end <- Gtk.textBufferGetEndIter buf
    value <- Gtk.textBufferGetText buf start end True
    return value

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

  screen <- maybe (fail "No screen?!") return =<< GDK.screenGetDefault
  css <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData css styles
  Gtk.styleContextAddProviderForScreen screen css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  img1 <- Gtk.imageNewFromFile $ home ++ "/.local/img/cancel.png"

  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 ("<b>" <> "Done" <> "</b>")

  -- input <- Gtk.entryNew
  -- dialog <- Gtk.dialogNew
  -- comboBox <- Gtk.comboBoxNew
  -- blackRgba                         <- GDK.newZeroRGBA
  -- whiteRgba                         <- GDK.newZeroRGBA
  -- _                                 <- GDK.rGBAParse blackRgba "rgba(0,0,0,1.0)"
  -- _                                 <- GDK.rGBAParse whiteRgba "rgba(255,255,255,1.0)"

  lonLat <- Gtk.textViewNew
  Gtk.textViewSetEditable lonLat True
  textBufferLonLat <- Gtk.getTextViewBuffer lonLat
  Gtk.textBufferSetText textBufferLonLat (( "45.18,-93.32")) (-1)

  data1 <- textViewGetValue lonLat
  print data1

  textView <- Gtk.textViewNew
  Gtk.textViewSetEditable textView False

  textBuffer <- Gtk.getTextViewBuffer textView
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0

  Gtk.setBoxHomogeneous box False
  Gtk.boxPackStart box textView True True 0

  btn1 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn1 $ Just img1
  Gtk.widgetSetHexpand btn1 False
  on btn1 #clicked $ do
    putStrLn "User chose: Done"
    Gtk.widgetDestroy win

  on win #keyPressEvent $ \keyEvent -> do
    key <- keyEvent `Data.GI.Base.get` #keyval >>= GDK.keyvalToUnicode
    putStrLn $ "Key pressed: (" ++ show key ++ ")"
    if key == 27 then Gtk.mainQuit else pure ()
    return False

  grid <- Gtk.gridNew
  Gtk.gridSetColumnSpacing grid 10
  Gtk.gridSetRowSpacing grid 10
  Gtk.gridSetColumnHomogeneous grid True

  #attach grid btn1   0 0 1 1
  #attach grid label1 0 1 1 1
  #attach grid box 0 2 1 1
  #attach grid lonLat 0 3 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  astroObs <- getAstroObservation
  obs <- getWeatherObservation

  let temperature = "Temperature: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperature |]))  ++ "\n"
  let pressure = "Pressure: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.pressureAltimeter |])) ++ "\n"
  let windChill = "WindChill: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureWindChill |])) ++ "\n"
  let windGust = "WindGust: " ++ (show (fromIntJust (head ([Data.Aeson.Schema.get| obs.values[].currentObservation.windGust |])))) ++ "\n"
  let windSpeed = "WindSpeed: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.windSpeed |])) ++ "\n"
  let heatIndex = "HeatIndex: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureHeatIndex |])) ++ "\n"
  let dewPoint = "DewPoint: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureDewPoint |])) ++ "\n"
  let precip1Hour = "Precipitation1: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip1Hour |])) ++ "\n"
  let precip6Hour = "Precipitation6: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip6Hour |])) ++ "\n"
  let precip24Hour = "Precipitation24: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip24Hour |])) ++ "\n"
  let sunRise = "Sunrise: " ++ unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.sunriseTimeLocal |]) ++ "\n"
  let sunSet = "Sunset: " ++ unpack(head [Data.Aeson.Schema.get| obs.values[].currentObservation.sunsetTimeLocal |]) ++ "\n"
  let phrase = "Phrase: " ++ unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.wxPhraseLong |]) ++ "\n"
  let uv = "UV: " ++  unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.uvDescription |]) ++ "\n"
  let uvIndex = "UV Index: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.uvIndex |])) ++ "\n"
  let windDirection = "Wind Direction: " ++ unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.windDirectionCardinal |]) ++ "\n"
  let feelsLike = "FeelsLike: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureFeelsLike |])) ++ "\n"
  let pressureTendency = "Pressure Tendency: " ++ unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.pressureTendencyTrend |]) ++ "\n"
  let cloudCover = "Cloud Cover: " ++ unpack (head [Data.Aeson.Schema.get| obs.values[].currentObservation.cloudCoverPhrase |]) ++ "\n"
  let snow1Hour = "Snow1Hour: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow1Hour |])) ++ "\n"
  let snow6Hour = "snow6Hour: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow6Hour |])) ++ "\n"
  let snow24Hour = "snow24Hour: " ++ (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow24Hour |])) ++ "\n"
  let moonRise = "Moon Rise: " ++ unpack (head [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.riseLocal |]) ++ "\n"
  let moonSet = "Moon Set: " ++ unpack (head [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.setLocal |]) ++ "\n"

  let myData = temperature ++ pressure ++ windChill ++ windGust ++ windSpeed ++ heatIndex ++ dewPoint ++ precip1Hour ++ precip6Hour ++ precip24Hour ++ sunRise ++ sunSet ++ phrase ++ uv ++ uvIndex ++ windDirection ++ feelsLike ++ pressureTendency ++ cloudCover ++ snow1Hour ++ snow6Hour ++ snow24Hour ++ moonRise ++ moonSet
  --
  Gtk.textBufferSetText textBuffer (pack( myData)) (-1)
  Gtk.main
