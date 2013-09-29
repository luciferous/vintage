module Main where

import Control.Monad.IO.Class (liftIO)

import Ping.GenTypes
import Vintage

app :: Session ()
app = do
    msg <- send Ping
    liftIO $ putStrLn (show (msg :: Pong))

main :: IO ()
main = runClient "localhost" 9090 app

