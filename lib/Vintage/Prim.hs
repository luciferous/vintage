module Vintage.Prim where

import Thrift.Protocol.Binary
import Thrift.Transport.Framed
import Thrift.Transport.Handle
import Vintage.Protocol.Binary
import Vintage.Types

import Control.Exception (catch, IOException)
import Control.Concurrent (forkFinally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Data.Binary (decode, encode, Binary)
import qualified Data.ByteString.Lazy as B
import Network (accept, connectTo, listenOn, PortID(PortNumber))
import System.IO (Handle)

type FramedBinaryPair = ( BinaryProtocol (FramedTransport Handle)
                        , BinaryProtocol (FramedTransport Handle)
                        )

type Session a = ReaderT FramedBinaryPair IO a

runClient :: String -> Int -> Session a -> IO a
runClient host port session = do
    transport <- hOpen (host, PortNumber $ fromIntegral port)
    framed <- openFramedTransport transport
    let proto = BinaryProtocol framed
    runReaderT session (proto, proto)

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

send :: (HasName a, HasName b, Binary a, Binary b) => a -> Session b
send req = do
    (i, o) <- ask
    liftIO $ do
        tWrite (getTransport o) (encode (Message req))
        tFlush (getTransport o)
        -- Fixme
        --bs <- tReadAll (getTransport i) 1024
        --return (decode bs)
        return (decode (encode req))
