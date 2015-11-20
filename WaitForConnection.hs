{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module WaitForConnection where

import Control.Concurrent
import System.Exit
import System.Process
import System.Time
    
--pingt google in a blocking way, ping geeft een ExitSuccess als de host bereikbaar was met alle pings, anders een false.
testConnection = do
    -- -n 1 zorgt er voor dat hij maar ��n request doet, dit douurt obviously veel korter dan vier met steeds een seconde ertussen
    (exitcode,_,_) <- readProcessWithExitCode "ping" ["www.google.com", "-c", "1"] [] --snekkie! op windows is ��n pakket -n, op linux -c
    return $ exitcode == ExitSuccess

-- worker method
waitForConnection' :: IO ()
waitForConnection' = do
    isConnectionUp <- testConnection
    if isConnectionUp then return () else (threadDelay 1000000) >> waitForConnection'
    
waitForConnection :: IO Integer
waitForConnection = do
    (TOD toen _) <- getClockTime
    waitForConnection'
    (TOD nu _) <- getClockTime
    return $ nu - toen