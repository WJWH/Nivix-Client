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
    setPinFunction Pin15 Output
    writePin Pin15 True
    chipSelectSPI CS0   --set the Chip select pin to the CS0 pin
    setChipSelectPolaritySPI CS0 False 
    setDataModeSPI (False,False) 
    waitForConnection
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
    where   (Just initreq) = parseUrl "http://192.168.178.20:3000" --poort 3000 op de laptop.

--shuts down, tenzij pin11 hoog is.
shutdown :: IO ()
shutdown = do
    setPinFunction Pin11 Input
    preventShutdown <- readPin Pin11 --voor debuggen is het handig om ook als er geen server is de pi te laten leven
    -- let preventShutdown = True
    if preventShutdown then return () else (writePin Pin15 False >> (createProcess $ shell "sudo shutdown -h now") >> return ())
    
--aeson werkt niet op de pi omdat je geen TH kan gebruiken. dit is natuurlijk een nogal brakke vervanger, maar je moet wat...
encode (STUW bat temp) = BSL.concat ["{\"tag\":\"STUW\",\"contents\":[", BSL.pack . show $ bat, ",", BSL.pack . show $ temp, "]}\""]

data Event = STUW Double Double