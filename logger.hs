{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Adclezer
import WaitForConnection

import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Conduit
import System.RaspberryPi.GPIO
import Network.HTTP.Types.Method
import System.Process


main = withGPIO . withSPI $ do --setup some things
    setPinFunction Pin15 Output --gaat naar de input van de ATTINY om aan te geven dat de pi aan staat
    setPinFunction Pin11 Input  --sensing pin om hardwarematig toch aan te blijven als de server geen keepalives geeft (bijv omdat hij uit staat)
    writePin Pin15 True --signaal naar de ATTINY dat de pi draait
    chipSelectSPI CS0   --set the Chip select pin to the CS0 pin
    setChipSelectPolaritySPI CS0 False 
    setDataModeSPI (False,False) 
    waitForConnection --wacht tot de wifi- dan wel de 3G-verbinding werkt
    battery <- readBattery
    temperature <- readTemperature
    shouldStayAlive <- postToNivix $ STUW battery temperature
    if shouldStayAlive then return () else shutdown
    
--Posts een request naar Nivix
postToNivix :: Event -> IO Bool
postToNivix evt = Control.Exception.handle (\(e :: HttpException) -> return False) $ do --als het faalt, dan sowieso weer gaan slapen
    req <- return $ makePostRequest (encode evt)
    manager <- newManager tlsManagerSettings
    resp <- httpLbs req manager
    return $ (responseBody resp) == "stayalive"

--parseURL werkt in een monad m die in de failure class zit, een voorbeeld van die class is good old Maybe.
makePostRequest :: BSL.ByteString -> Request
makePostRequest payload = initreq { method = methodPost , requestBody = RequestBodyLBS payload } --encode geeft een lazy BS
    where   (Just initreq) = parseUrl "http://templogger.snokums.com" --templogger user op snokums

--shuts down, tenzij pin11 hoog is.
shutdown :: IO ()
shutdown = do
    preventShutdown <- readPin Pin11 --voor debuggen is het handig om ook als er geen server is de pi te laten leven
    -- let preventShutdown = True
    if preventShutdown then return () else (writePin Pin15 False >> (createProcess $ shell "sudo shutdown -h now") >> return ())
    
--aeson werkt niet op de pi omdat je geen TH kan gebruiken. dit is natuurlijk een nogal brakke vervanger, maar je moet wat...
encode (STUW bat temp) = BSL.concat ["{\"tag\":\"STUW\",\"contents\":[", BSL.pack . show $ bat, ",", BSL.pack . show $ temp, "]}"]

data Event = STUW Double Double