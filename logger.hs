import Adclezer
import WaitForConnection

import Network.HTTP.Conduit
import System.RaspberryPi.GPIO


main = withGPIO . withSPI $ do --setup some things
    chipSelectSPI CS0   --set the Chip select pin to the CS0 pin
    setChipSelectPolaritySPI CS0 False 
    setDataModeSPI (False,False) 
    measurement <- doMeasurement 
    waitForConnection
    battery <- readBattery
    temperature <- readTemperature
    shouldStayAlive <- postToNivix $ SENSOR battery temperature
    if shouldStayAlive then return () else shutdown
    
--Posts een request naar
postToNivix :: Event -> IO Bool
postToNivix evt = handle (\(e :: HttpException) -> return false) $ do --als het faalt, dan sowieso weer gaan slapen
    req <- makePostRequest $ encode evt
    Response bs <- withManager $ \manager -> httpLbs req manager
    return $ bs == "stayalive"
    
shutdown = shell "sudo shutdown -h now"

--parseURL werkt in een monad m die in de failure class zit, een voorbeeld van die class is good old Maybe.
makePostRequest :: BSL.ByteString -> Request
makePostRequest payload = initreq { method = POST, requestBody = RequestBodyLBS payload } --encode geeft een lazy BS
    where (Just initreq) = parseUrl "http://192.168.178.50:3000" --poort 3000 op de vaste pi.