module Adclezer where

import System.RaspberryPi.GPIO
import Control.Monad
import Control.Concurrent
import Data.Bits
import Debug.Trace

--idee: interfacen met de A/D converter

readTemperature :: IO Double --leest de thermistor en geeft de waarde terug
readTemperature = do
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,128,0]) -- doe het vijf keer
    return . (myRound 1) . average . (map extractTemperatureResults) $ results
    
readBattery :: IO Double --leest de spanning van de batterij
readBattery = do
    writePin Pin13 True --zet de spanningsdeler aan
    threadDelay 5000 --wacht een paar milliseconden om de spannings laten stabiliseren (nodig????)
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,144,0]) -- doe het vijf keer
    return . (myRound 2) . average . (map extractBatteryResults) $ results
    writePin Pin13 False --zet de spanningsdeler weer uit om stroom te besparen
    
--de spanning van de batterij ligt ergens tussen drie en 4.2V?
--de reference spanning van de ADC is maar 3.3V, dus een spanningsdeler van 2 10kOhm weerstanden haalt de spanning ver genoeg naar beneden om goed te meten.
--je moet wel aan het eind de gemeten waarde weer met twee vermenigvuldigen om de waarde van de batterij ipv de waarde halverwege de spanningsdeler te krijgen
extractBatteryResults :: (Bits a, Integral a) => [a] -> Double
extractBatteryResults [_,p,q] = 2 * adcresult * (refvoltage/1023)
    where   adcresult = ((fromIntegral (p .&. 0x3)) * 256) + (fromIntegral q)
            refvoltage = 3.3 --de 3.3 rail van de pi

--de formule komt van adafruit en wikipedia. 
extractTemperatureResults :: (Bits a, Integral a) => [a] -> Double -- 
extractTemperatureResults [_,p,q] = t - 273.15 -- terugrekenen van Kelvin naar Celsius
    where   t = 1/( (1/t0) + ((1/b)* (log (thermistorweerstand / r0)))) -- zie https://learn.adafruit.com/thermistor/using-a-thermistor
            thermistorweerstand = (adcresult * seriesweerstand) / (1023 - adcresult)
            adcresult = ((fromIntegral (p .&. 0x3)) * 256) + (fromIntegral q)
            seriesweerstand = 9870.0 --gemeten met de multimeter
            t0 = 298.15 -- 25 graden celsius in Kelvin, nominale temperatuur van de thermistor waarbij de thermistor de nominale weerstand heeft
            r0 = 10000  -- 10 kOhm, nominale weerstand van de thermistor bij 25 graden C
            b = 3950    -- coefficient van de thermistor
extractTemperatureResults _ = error "extractTemperatureResults kreeg een inputlijst die niet uit drie elementen bestond."

myRound :: Integer -> Double -> Double
myRound places n = (fromIntegral $ round (n * factor)) / factor
    where factor = 10 ^ places

average :: [Double] -> Double
average [] = 0
average xs = (sum xs) / (fromIntegral $ length xs)
