import Control.Concurrent ( threadDelay )
import Control.Monad ( forever, void )
import Control.Monad.State ( StateT, get, liftIO, put, runStateT )
import Data.List ( intercalate )
import Data.Maybe ( mapMaybe )
import Text.XML.Light ( Element, QName(QName), findAttr, findElement, findElements, parseXML, onlyElems )
import Network.HTTP ( Request, simpleHTTP, getRequest, getResponseBody )
import Network.URI ( escapeURIString, isUnreserved )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitFailure), exitWith )
import System.IO ( hPutStrLn, stderr )

-- General utilities

average :: (Num a, Fractional a) => [a] -> a
average xs = sum xs / (fromIntegral $ length xs)

-- XML utilities

simpleName :: String -> QName
simpleName name = QName name Nothing Nothing

findAttr' :: String -> Element -> Maybe String
findAttr' = findAttr . simpleName

findElement' :: String -> Element -> Maybe Element
findElement' = findElement . simpleName

findElements' :: String -> Element -> [Element]
findElements' = findElements . simpleName

-- API access

type Query = [(String, String)]
type URL = String

escapeURL :: URL -> URL
escapeURL = escapeURIString isUnreserved

buildURL :: Query -> URL
buildURL = ("http://weather.service.msn.com/find.aspx?" ++) . encodeQuery
  where encodeQuery :: Query -> String
        encodeQuery = intercalate "&" . map (\(a, b) -> a ++ "=" ++ escapeURL b)

type Location = String
type Temperature = Double

doRequest :: Location -> IO [Element]
doRequest location =
  (onlyElems . parseXML) <$> (getResponseBody =<< simpleHTTP request)
  where url :: URL
        url = buildURL [ ("src", "outlook")
                       , ("culture", location)
                       , ("weadegreetype", "C")
                       , ("weasearchstr", location)
                       ]
        request :: Request String
        request = getRequest url

getTemperature :: Location -> IO Temperature
getTemperature location = do
  weatherData <- doRequest location
  let locations = concatMap (findElements' "weather") weatherData
      temperatures = mapMaybe getLocationTemperature locations
  return $ average temperatures
    where getLocationTemperature :: Element -> Maybe Temperature
          getLocationTemperature location =
            read <$> (findElement' "current" location >>= findAttr' "temperature")

-- Main

threshold :: Temperature
threshold = 20

data FanState = Off | On

type FanIO = StateT FanState IO

fanOn :: FanIO ()
fanOn = do
  state <- get
  case state of
    Off -> do
      liftIO $ writeFile "/sys/bus/usb/drivers/usb/bind" "1-1"
      put On
    On -> return ()

fanOff :: FanIO ()
fanOff = do
  state <- get
  case state of
    On -> do
      liftIO $ writeFile "/sys/bus/usb/drivers/usb/unbind" "1-1"
      put Off
    Off -> return ()

main' :: Location -> FanIO ()
main' location = forever $ do
  temperature <- liftIO $ getTemperature location
  if temperature > threshold
     then fanOn
     else fanOff
  liftIO $ threadDelay 300000000 -- 5 minutes as microseconds

incorrectUsage :: String -> IO ()
incorrectUsage message = do
  hPutStrLn stderr message
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> incorrectUsage "Place name should be passed as a command-line argument"
    [location] -> void $ runStateT (main' location) Off
    _ -> incorrectUsage "Too many arguments"
