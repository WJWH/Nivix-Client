{-# LANGUAGE ScopedTypeVariables #-}
module WaitForConnection where

import Control.Concurrent
import qualified Control.Exception as CE
import Network.HTTP.Conduit
import System.Time

--doet precies wat het zegt
testConnection :: IO Bool
testConnection = do
    (simpleHttp "https://www.google.com" >> return True) `CE.catch` (\(e :: HttpException) -> return False) --TODO: simpleHttp is super wasteful, een ping method zou beter zijn
    
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