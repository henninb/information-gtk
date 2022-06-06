{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as GDK
import System.Directory
import System.Posix.User
import System.Process
-- import Data.Text as Text
import Data.Char (chr)
import Data.Aeson
import GHC.Generics
import Data.String ( fromString )
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Servant
import System.IO
import Network.HTTP.Req as Req
import Control.Monad.IO.Class
import Requests
import Data.HashMap
-- import qualified RIO.ByteString   as B
import qualified Data.ByteString.Lazy as B
import Text.JSON.Generic
-- import qualified Handlers.Logger            as L

-- data Imperial = Imperial {
--   temp :: Integer
-- } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)


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

-- data Observation  = Observation {
--   stationID :: String,
--   obsTimeUtc :: String,
--   imperial :: Object
-- } deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Observation  = Observation {
  stationID :: String,
  obsTimeUtc :: String,
  neighborhood :: String,
  -- softwareType :: String,
  country:: String,
  solarRadiation :: Double,
  lon :: Double,
  -- realtimeFrequency :: String,
  epoch :: Integer,
  lat :: Double,
  uv :: Integer,
  winddir :: Integer,
  humidity :: Integer,
  qcStatus :: Integer,
  imperial :: Imperial
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Observations = Observations {
  observations :: Observations
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Category = Category
    {foo :: Integer,
    bar  :: Integer
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data Status = Status { ok :: Bool }
    deriving (Generic)

instance FromJSON Status


-- type CategoryApi =
--   Get '[JSON] String
--   :<|> "category" :> Get '[JSON] [Category]
--   :<|> "category" :> Capture "id" Integer :> Get '[JSON] Category
--   :<|> "optional" :> QueryParam "parameter1" Int :> Get '[JSON] String  -- equivalent to 'GET /optional?parameter1=test'

newtype GetSeriesResponse = GetSeriesResponse {
  seriesName :: String
} deriving (Show)

instance FromJSON GetSeriesResponse where
  parseJSON = withObject
    ""
    (\o -> GetSeriesResponse <$> (o .: "data" >>= flip (.:) "seriesName"))


exampleObservation = "{\"observations\":[{\"stationID\":\"KMNCOONR65\",\"obsTimeUtc\":\"2022-06-05T20:18:00Z\",\"obsTimeLocal\":\"2022-06-05 15:18:00\",\"neighborhood\":\"Thompson Heights\",\"softwareType\":null,\"country\":\"US\",\"solarRadiation\":57.3,\"lon\":-93.306,\"realtimeFrequency\":null,\"epoch\":1654460280,\"lat\":45.193,\"uv\":0.0,\"winddir\":187,\"humidity\":53,\"qcStatus\":1,\"imperial\":{\"temp\":71,\"heatIndex\":70,\"dewpt\":53,\"windChill\":71,\"windSpeed\":3,\"windGust\":3,\"pressure\":29.76,\"precipRate\":0.00,\"precipTotal\":0.00,\"elev\":863}}]}"

eitherObsersation = eitherDecode exampleObservation :: Either String Observation
-- exampleCategory :: String
exampleCategory = "{ \"foo\": 1, \"bar\": 2 }"

eitherCategory = eitherDecode exampleCategory :: Either String Category


jsonFile :: FilePath
jsonFile = "example.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- exampleObservation1 = "{\"observations\": [{ \"stationID\": \"abc\", \"obsTimeUtc\": \"2022-06-05T20:18:00Z\" }]}"
-- exampleObservation1 = "[{ \"stationID\": \"abc\", \"obsTimeUtc\": \"2022-06-05T20:18:00Z\", \"junk\":1, \"imperial\":{\"temp\":71}}]"
exampleObservation1 = "[{\"stationID\":\"KMNCOONR65\",\"obsTimeUtc\":\"2022-06-05T20:18:00Z\",\"obsTimeLocal\":\"2022-06-05 15:18:00\",\"neighborhood\":\"Thompson Heights\",\"softwareType\":null,\"country\":\"US\",\"solarRadiation\":57.3,\"lon\":-93.306,\"realtimeFrequency\":null,\"epoch\":1654460280,\"lat\":45.193,\"uv\":0.0,\"winddir\":187,\"humidity\":53,\"qcStatus\":1,\"imperial\":{\"temp\":71,\"heatIndex\":70,\"dewpt\":53,\"windChill\":71,\"windSpeed\":3,\"windGust\":3,\"pressure\":29.76,\"precipRate\":0.00,\"precipTotal\":0.00,\"elev\":863}}]"
eitherObsersation1 = eitherDecode exampleObservation1 :: Either String [Observation]

-- loadApiConfigs :: IO Observation
-- loadApiConfigs = do
--     config <- (eitherDecode <$> getJSON) :: IO (Either String Observation)
--     print config
--     -- print "gothere"
--     -- liftIO (print config)
--     -- print "gothere"
--     case config of
--         Left _ -> empty
--         Right x -> return x

-- parse :: FromJSON a => ByteString -> Maybe [Either Value a]
-- parse s = case decode s of
--   Nothing -> fail "could not decode as array"
--   Just values -> map tryDecode values
-- where
--   tryDecode :: FromJSON a =>Value -> Either Value a
--   tryDecode v = case decode (encode v) of
--     Nothing -> Left v
--     Just a -> Right a

-- parseResponse :: (FromJSON response, MonadThrow m, Show response) => L.Handle m -> B.ByteString -> m response
-- parseResponse hLogger respBody = case eitherDecode respBody of
--     Right result -> do
--         L.debug hLogger (show result)
--         return result
--     Left e     -> do
--         L.error hLogger $ ("Parsing failed due to mismatching type, error:\n\t" <> fromString e <> "\n") <> BC.unpack respBody
--         throwM $ RParseException . WrongType . fromString $ e

-- test =
--   case eitherDecode exampleObservation of
--     Right ret -> do
--       debug $ "Observation returned perms: " ++ (show ret)
--       return ret
--     Left (err2 :: String) -> do
--       debug $ "Keycloak parse error: " ++ (show err2)
--       throwError $ ParseError $ pack (show err2)

-- test :: Either String Category
-- test = eitherDecode exampleJson

-- getExtra :: Object -> Parser Object
-- getExtra v = do
--   mextra <- v .:? "extra"
--   case mextra of
--     Just vv -> vv & withArray "Abc.extra" (\arr -> do
--       let vallst = toList arr
--       objlst <- traverse (withObject "Abc.extra[..]" pure) vallst
--       return $ HashMap.unions objlst)
--     Nothing -> return HashMap.empty


getSeriesRequest :: Int -> Request NoReqBody GetSeriesResponse GET
getSeriesRequest seriesId =
  Request NoReqBody GET [] $ https "api.thetvdb.com" /: "series" /: fromString
    (show seriesId)

-- getEpisodesRequest :: Int -> Int -> Request NoReqBody Category GET
-- getEpisodesRequest seriesId page =
--   Request NoReqBody GET [("page", show page)]
--     $  https "api.thetvdb.com"
--     /: "series"
--     /: fromString (show seriesId)
--     /: "episodes"

-- getMoves IO Value :: IO Value
getMoves mv = do
  -- mv <- decode <$> B.readFile "moves.json"
  case mv of
    Nothing -> error "invalid JSON"
    Just v -> return v

examplePost :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
examplePost = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  response <-
    req
      POST -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
      -- return $ ok (responseBody res :: Status)
  liftIO $ print (responseBody response :: Value)


exampleGet :: IO ()
exampleGet = runReq defaultHttpConfig $ do
  response <-
    req
      GET -- method
      (https "api.weather.com" /: "v2" /: "pws" /: "observations" /: "current")
      NoReqBody
      jsonResponse $
      "apiKey" =: ("e1f10a1e78da46f5b10a1e78da96f525" :: String) <>
      "units" =: ("e" :: String) <>
      "stationId" =: ("KMNCOONR65" :: String) <>
      "format" =: ("json" :: String)
  liftIO $ print (responseBody response :: Value)
  -- return $ ok (responseBody response :: Status)

-- exampleGetNew = runReq defaultHttpConfig $ do
--   response <-
--     req
--       GET -- method
--       (https "api.weather.com" /: "v2" /: "pws" /: "observations" /: "current")
--       NoReqBody
--       jsonResponse $
--       "apiKey" =: ("e1f10a1e78da46f5b10a1e78da96f525" :: String) <>
--       "units" =: ("e" :: String) <>
--       "stationId" =: ("KMNCOONR65" :: String) <>
--       "format" =: ("json" :: String)
--     return $ ok (responseBody response :: Status)
  -- liftIO $ print (responseBody response :: Value)

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
    putStrLn "post"
    examplePost
    putStrLn "get"
    exampleGet
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

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win
  let Right unwrappedCategory = eitherCategory
  print unwrappedCategory

  putStrLn $ "load configs"
  let Right unwrappedObservation1 = eitherObsersation1
  print (unwrappedObservation1)
  -- print (length unwrappedObservation1)
  -- let record = (head unwrappedObservation1)
  -- print $ imperial record
  -- let myImperial = imperial record
  -- print $ temp $ imperial record
  putStrLn $ "load configs"
  -- let Right unwrappedObservation = eitherObsersation
  -- print unwrappedObservation
  -- debug $ "Keycloak returned perms: " ++ (show unwrappedObservation)
  -- x <- decode exampleObservation :: Maybe Observation
  -- print x
  -- liftIO (getSeriesRequest)

  Gtk.main
