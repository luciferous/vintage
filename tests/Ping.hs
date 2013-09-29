module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import Ping.GenTypes
import Vintage

app :: Session ()
app = do
    msg <- send Ping
    liftIO $ putStrLn (show (msg :: Pong))

server :: Ping -> IO Pong
server Ping = return Pong

main :: IO ()
main = getArgs >>= \args -> case args of
    ["client"] -> runClient "localhost" 9090 app
    ["server"] -> runServer 9090 server
    _          -> putStrLn "Usage: pingtest [client|server]"

