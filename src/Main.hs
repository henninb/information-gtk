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

-- styles :: BL.ByteString
styles = mconcat
    [ ".title { font-size: large; margin: 5pt 10pt; }"
    , ".exercise_reps { font-size: xx-large; font-family: monospace; margin: 10pt 5pt 15pt 5pt; }"
    , "button { font-size: large; margin: 2pt 8pt; }"
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
  p <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen screen p (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)


  img1 <- Gtk.imageNewFromFile $ home ++ "/.local/img/cancel.png"

  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 ("<b>" <> "Done" <> "</b>")

  temperatureLabel <- Gtk.labelNew Nothing

  pressureLabel <- Gtk.labelNew Nothing
  label5 <- Gtk.labelNew Nothing
  label6 <- Gtk.labelNew Nothing
  label7 <- Gtk.labelNew Nothing
  label8 <- Gtk.labelNew Nothing
  label9 <- Gtk.labelNew Nothing
  label10 <- Gtk.labelNew Nothing
  label11 <- Gtk.labelNew Nothing
  label12 <- Gtk.labelNew Nothing
  label13 <- Gtk.labelNew Nothing
  label14 <- Gtk.labelNew Nothing
  label15 <- Gtk.labelNew Nothing
  label16 <- Gtk.labelNew Nothing
  label17 <- Gtk.labelNew Nothing
  label18 <- Gtk.labelNew Nothing
  label19 <- Gtk.labelNew Nothing
  label20 <- Gtk.labelNew Nothing
  label21 <- Gtk.labelNew Nothing
  label22 <- Gtk.labelNew Nothing
  label23 <- Gtk.labelNew Nothing
  label24 <- Gtk.labelNew Nothing
  label25 <- Gtk.labelNew Nothing
  label26 <- Gtk.labelNew Nothing
  label27 <- Gtk.labelNew Nothing
  label28 <- Gtk.labelNew Nothing
  label29 <- Gtk.labelNew Nothing

  -- newFontDesc      <- GRPF.fontDescriptionNew
  -- let fontDesc     = fromMaybe newFontDesc textOverlayMaybeFontDesc
  -- fontFamily       <-                       fromMaybe ""                 <$> GRPF.fontDescriptionGetFamily  fontDesc
  -- fontStyle        <- removeQuotes . show . fromMaybe GRPF.StyleNormal   <$> GRPF.fontDescriptionGetStyle   fontDesc
  -- fontStretch      <- removeQuotes . show . fromMaybe GRPF.StretchNormal <$> GRPF.fontDescriptionGetStretch fontDesc
  -- fontWeight       <-                                                        getFontWeight                  fontDesc
  -- fontSize         <-                       fromMaybe 30.0               <$> GRPF.fontDescriptionGetSize    fontDesc
  -- f <- Gtk.box Nothing
  -- only_grid <- Gtk.gridNew
  -- x <- Gtk.boxNew Nothing
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0
  input <- Gtk.entryNew
  -- tt <- Gtk.newTextArea
  -- textbuf <- Gtk.textBufferNew Gtk.OrientationHorizontal 0
  textView <- Gtk.textViewNew
  Gtk.textViewSetEditable textView False

  textBuffer <- Gtk.getTextViewBuffer textView
  Gtk.textBufferSetText textBuffer "matthew\nmaggie" (-1)
  -- Gtk.textBufferSetText textBuffer "maggie" (-1)
  Gtk.textViewSetBuffer textView (Just textBuffer)

  -- Gtk.textViewSetMarkup textView ""
  -- Gtk.textViewSetBuffer textView (Just "test")
  -- Gtk.textViewSetBuffer textView Nothing
  -- textview1.Buffer.Text = "Some sample text that will be displayed."
  buf <- Gtk.textViewGetBuffer textView
  dialog <- Gtk.dialogNew
  -- vbox <- Gtk.vBoxNew False 0
  -- hbox <- Gtk.hBoxNew False 0
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0

  Gtk.setBoxHomogeneous box False
  Gtk.boxPackStart box textView True True 0


  blackRgba                         <- GDK.newZeroRGBA
  whiteRgba                         <- GDK.newZeroRGBA
  _                                 <- GDK.rGBAParse blackRgba "rgba(0,0,0,1.0)"
  _                                 <- GDK.rGBAParse whiteRgba "rgba(255,255,255,1.0)"
  -- txtbuf <- Gtk.textBufferNew
  -- txtview <- Gtk.textViewNewWithBuffer txtbuf
  -- cbx <- Gtk.comboBoxNewText
  -- vbox <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)

  -- temperatureLabel.modifyFont  "test"
  -- Gtk.labelModifyFont label5 "test"
  -- Gtk.widgetModifyFont label5 "test"

  -- textbuf <- Gtk.textBufferNew Nothing
  -- Gtk.widgetSetSizeRequest textarea 500 600
  -- fdesc <- Gtk.fontDescriptionNew Nothing
  -- Gtk.fontDescriptionSetFamily fdesc ("Mono" :: String)
  -- Gtk.widgetModifyFont textarea (Just fdesc)

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
  #attach grid temperatureLabel 1 1 1 1
  #attach grid pressureLabel 1 2 1 1
  #attach grid label5 1 3 1 1
  #attach grid label6 1 4 1 1
  #attach grid label7 1 5 1 1
  #attach grid label8 1 6 1 1
  #attach grid label9 1 7 1 1
  #attach grid label10 1 8 1 1
  #attach grid label11 1 9 1 1
  #attach grid label12 1 10 1 1
  #attach grid label13 1 11 1 1
  #attach grid label14 1 12 1 1
  #attach grid label15 1 13 1 1
  #attach grid label16 1 14 1 1
  #attach grid label17 1 15 1 1
  #attach grid label18 1 16 1 1
  #attach grid label19 1 17 1 1
  #attach grid label20 1 18 1 1
  #attach grid label21 1 19 1 1
  #attach grid label22 1 20 1 1
  #attach grid label23 1 21 1 1
  #attach grid label24 1 22 1 1
  #attach grid label25 1 23 1 1
  #attach grid label26 1 24 1 1
  #attach grid label27 1 25 1 1
  -- #attach grid textView 0 2 1 1
  #attach grid box 0 2 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  astroObs <- getAstroObservation
  obs <- getWeatherObservation
  -- observation <- getObservation
  -- let imperialData = (imperial observation)
  Gtk.labelSetMarkup temperatureLabel ("<b>" <> "Temperature: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperature |])) <> " F" <> "</b>")
  Gtk.labelSetMarkup pressureLabel ("<b>" <> "Pressure: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.pressureAltimeter |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label5 ("<b>" <> "WindChill: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureWindChill |])) <> "" <> "</b>")
  -- bh - wind gust returns a Just
  let windGustJust = (head ([Data.Aeson.Schema.get| obs.values[].currentObservation.windGust |]))
  let windGust = fromIntJust windGustJust
  Gtk.labelSetMarkup label6 ("<b>" <> "WindGust: " <> pack (show (windGust)) <> "" <> "</b>")
  -- Gtk.labelSetMarkup label6 ("<b>" <> "WindGust: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.windGust |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label7 ("<b>" <> "WindSpeed: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.windSpeed |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label8 ("<b>" <> "HeatIndex: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureHeatIndex |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label9 ("<b>" <> "DewPoint: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureDewPoint |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label10 ("<b>" <> "Precipitation1: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip1Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label11 ("<b>" <> "Precipitation6: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip6Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label12 ("<b>" <> "Precipitation24: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.precip24Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label13 ("<b>" <> "Sunrise: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.sunriseTimeLocal |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label14 ("<b>" <> "Sunset: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.sunsetTimeLocal |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label15 ("<b>" <> "Phrase: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.wxPhraseLong |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label16 ("<b>" <> "UV: " <>  (head [Data.Aeson.Schema.get| obs.values[].currentObservation.uvDescription |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label17 ("<b>" <> "UV Index: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.uvIndex |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label18 ("<b>" <> "Wind Direction: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.windDirectionCardinal |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label19 ("<b>" <> "FeelsLike: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureFeelsLike |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label20 ("<b>" <> "Pressure Tendency: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.pressureTendencyTrend |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label21 ("<b>" <> "Cloud Cover: " <> (head [Data.Aeson.Schema.get| obs.values[].currentObservation.cloudCoverPhrase |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label22 ("<b>" <> "Snow1Hour: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow1Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label23 ("<b>" <> "snow6Hour: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow6Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label24 ("<b>" <> "snow24Hour: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.snow24Hour |])) <> "" <> "</b>")
  Gtk.labelSetMarkup label25 ("<b>" <> "Moon Rise: " <> (head [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.riseLocal |]) <> "" <> "</b>")
  Gtk.labelSetMarkup label26 ("<b>" <> "Moon Set: " <> (head [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.setLocal |]) <> "" <> "</b>")
  -- Gtk.labelSetMarkup label22 ("<b>" <> "NothingYet: " <> pack (show (head [Data.Aeson.Schema.get| obs.values[].currentObservation.something |])) <> "" <> "</b>")

  Gtk.main
