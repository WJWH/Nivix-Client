module Adclezer where

import System.RaspberryPi.GPIO
import Control.Monad
import Control.Concurrent
import Data.Bits
import Debug.Trace

-- De A/D converter verwacht een serie bytes over de SPI bus als [1,x,0].
-- De eerste 1 komt omdat het een 0x1 is die de MCP verwacht als startbit. Dan komt
-- 1 bit die de single/differential meetmethode is, je wil vrijwel altijd single.
-- De volgende 3 geeft aan welke pin je van wil meten, de laatste vier bits van de tweede
-- byte zijn "don't care". Dan komen nog eens 8 "don't care" bits die nodig zijn om de
-- clock pin op active te houden. Daarvoor gebruiken we 0 omdat het toch niet uitmaakt.
-- Zie ook blz 21 van de MCP3008 datasheet


readTemperature :: IO Double --leest de thermistor en geeft de temperatuur terug
readTemperature = do
    -- doe het vijf keer
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,128,0]) -- 128 = 128 + (pin 0 << 4)
    return . (myRound 1) . average . (map extractTemperatureResults) $ results

readBattery :: IO Double --leest de spanning van de batterij
readBattery = do
    threadDelay 5000 --wacht een paar milliseconden om de spannings laten stabiliseren (nodig????)
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,144,0]) -- 160 = 128 + 32 = 128 + (pin 1 << 4)
    return . (myRound 2) . average . (map extractBatteryResults) $ results

readRaw :: IO Double -- leest direct de spanning en rondt die af zodat je serverside er iets mee kan doen
    results <- replicateM 5 (threadDelay 10000 >> transferManySPI [1,160,0]) -- 160 = 128 + 32 = 128 + (pin 2 << 4)
    return . (myRound 2) . average $ results

--de spanning van de batterij ligt ergens tussen drie en 4.2V?
--de reference spanning van de ADC is maar 3.3V, dus een spanningsdeler van 2 10kOhm weerstanden haalt de spanning ver genoeg naar beneden om goed te meten.
--je moet wel aan het eind de gemeten waarde weer met twee vermenigvuldigen om de waarde van de batterij ipv de waarde halverwege de spanningsdeler te krijgen
extractBatteryResults :: (Bits a, Integral a) => [a] -> Double
extractBatteryResults [_,p,q] = 2 * adcresult * (refvoltage/1023)
    where   adcresult = ((fromIntegral (p .&. 0x3)) * 256) + (fromIntegral q)
            refvoltage = 3.3 --de 3.3 rail van de pi

--de formule komt van adafruit en wikipedia.
extractTemperatureResults :: (Bits a, Integral a) => [a] -> Double
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
