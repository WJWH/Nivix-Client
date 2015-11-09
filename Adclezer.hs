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
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,144,0]) -- doe het vijf keer
    return . (myRound 2) . average . (map extractBatteryResults) $ results
    
extractBatteryResults :: (Bits a, Integral a) => [a] -> Double
extractBatteryResults [_,p,q] = (adcresult/1023)*refvoltage
    where   adcresult = ((fromIntegral (p .&. 0x3)) * 256) + (fromIntegral q)
            refvoltage = 5.2
    
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
