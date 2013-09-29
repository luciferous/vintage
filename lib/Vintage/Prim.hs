module Vintage.Prim where

import Control.Exception (catch, IOException)
import Control.Concurrent (forkFinally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Binary (decode, encode, Binary)
import qualified Data.ByteString.Lazy as B
import Network (accept, connectTo, listenOn, PortID(PortNumber))
import System.IO (Handle)

import Vintage.Types

runClient :: String -> Int -> Session a -> IO a
runClient host port session = do
    conn <- connectTo host (PortNumber $ fromIntegral port)
    runReaderT session conn

runServer :: (Binary a, Binary b) => Int -> (a -> IO b) -> IO ()
runServer port app = do
    sock <- listenOn (PortNumber $ fromIntegral port)
    go sock
  where
    go sock = do
        (handle, _, _) <- accept sock
        catch (receive handle >>= app >>= respond handle)
              (\e -> putStrLn (show (e :: IOException)))
        go sock

receive :: Binary a => Handle -> IO a
receive handle = do
    len <- B.hGet handle 8
    payload <- B.hGet handle (decode len)
    return (decode payload)


respond :: Binary b => Handle -> b -> IO ()
respond handle payload = do
    let message = encode payload
    B.hPut handle (encode (B.length message))
    B.hPut handle message 

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
