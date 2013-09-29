module Vintage.Prim where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Binary (decode, encode, Binary)
import qualified Data.ByteString.Lazy as B
import Network (connectTo, PortID(PortNumber))

import Vintage.Types

runClient :: String -> Int -> Session a -> IO a
runClient host port session = do
    conn <- connectTo host (PortNumber $ fromIntegral port)
    runReaderT session conn

send :: (Binary a, Binary b) => a -> Session b
send req = do
    conn <- ask
    liftIO $ do
        let message = encode req
        B.hPut conn (encode (B.length message))
        B.hPut conn message
        len <- B.hGet conn 8
        payload <- B.hGet conn (decode len)
        return (decode payload)
