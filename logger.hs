{-# LANGUAGE OverloadedStrings #-}
import Adclezer
import WaitForConnection

import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Conduit
import System.RaspberryPi.GPIO
import Network.HTTP.Types.Method


main = withGPIO . withSPI $ do --setup some things
    chipSelectSPI CS0   --set the Chip select pin to the CS0 pin
    setChipSelectPolaritySPI CS0 False 
    setDataModeSPI (False,False) 
    measurement <- doMeasurement 
    waitForConnection
    battery <- return 100 --readBattery
    temperature <- readTemperature
    shouldStayAlive <- postToNivix $ STUW battery temperature
    if shouldStayAlive then return () else shutdown
    
--Posts een request naar Nivix
postToNivix :: Event -> IO Bool
postToNivix evt = handle (\(e :: HttpException) -> return False) $ do --als het faalt, dan sowieso weer gaan slapen
    req <- makePostRequest $ encode evt
    manager <- newManager tlsManagerSettings
    Response bs <- httpLbs req manager
    return $ bs == "stayalive"

--parseURL werkt in een monad m die in de failure class zit, een voorbeeld van die class is good old Maybe.
makePostRequest :: BSL.ByteString -> Request
makePostRequest payload = initreq { method = POST, requestBody = RequestBodyLBS payload } --encode geeft een lazy BS
    where   (Just initreq) = parseUrl "http://192.168.178.20:3000" --poort 3000 op de laptop.

--shuts down, tenzij pin11 hoog is.
shutdown :: IO ()
shutdown = do
    setPinFunction Pin11 Input
    preventShutdown <- readPin Pin11 --voor debuggen is het handig om ook als er geen server is de pi te laten leven
    if preventShutdown then return () else spawnCommand "sudo shutdown -h now"
    
--aeson werkt niet op de pi omdat je geen TH kan gebruiken. dit is natuurlijk een nogal brakke vervanger, maar je moet wat...
encode (STUW _ temp) = BSL.concat ["{\"tag\":\"STUW\",\"contents\":[", "100", ",", BSL.pack . show $ temp, "]}\""]

data Event = STUW Double Double