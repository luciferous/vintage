{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Vintage.Protocol.Binary where

import Control.Applicative ((<*), (<*>), (*>), (<$>), pure)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad (when)

import Thrift

newtype TList a = TList { list :: (ThriftType, [a]) } deriving Show

data Message a = Message a deriving Show

type StringType = T.Text

type SeqId = Word32

instance Show ThriftType where
    show = show . fromEnum

instance Binary T.Text where
    get = getString
    put = putString

class HasName a where
    nameOf :: a -> StringType

instance (HasName a, Binary a) => Binary (Message a) where
    put (Message a) = putMessageBegin (nameOf a) M_CALL 0
                   *> put a
                   *> putMessageEnd
    get = Message <$> (getMessageBegin *> get) <* getMessageEnd

instance Binary a => Binary (TList a) where
    get = do
        (ttype, len) <- getListBegin
        items <- getMany (fromIntegral len)
        return $ TList (ttype, items)
    put tlist = putListBegin ttype (fromIntegral len)
             *> mapM_ put items
             *> putListEnd
      where
        ttype = (fst . list) tlist
        items = (snd . list) tlist
        len   = length items

-- * ThriftTypeable

class ThriftTypeable a where
    thriftTypeOf :: a -> ThriftType

instance ThriftTypeable StringType where
    thriftTypeOf _ = T_STRING

instance ThriftTypeable Word16 where
    thriftTypeOf _ = T_I16

instance ThriftTypeable [a] where
    thriftTypeOf _ = T_LIST

instance ThriftTypeable (TList a) where
    thriftTypeOf _ = T_LIST

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
-- Copied from Data.Binary. Remove when kolmodin/binary/pull/39 is pulled.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

fromList :: ThriftTypeable a => [a] -> TList a
fromList xs = TList (elementType, xs)
  where elementType = (thriftTypeOf . head) xs

struct :: StringType -> Put -> Put
struct name contents = putStructBegin name
                    *> contents 
                    *> putFieldStop
                    *> putStructEnd

-- *Put

-- | Put field
putField :: (ThriftTypeable a, Binary a) => (Word16, T.Text, a) -> Put
putField (ix, name, value) = putFieldBegin name (thriftTypeOf value) ix
                          *> put value
                          *> putFieldEnd


-- | Put message
putMessageBegin :: StringType -> MessageType -> SeqId -> Put
putMessageBegin name mtype seqid = putWord32be vid 
                                *> putString name
                                *> putWord32be seqid
  where vid = v1 .|. (fromIntegral . fromEnum $ mtype)
putMessageEnd :: Put
putMessageEnd = pure ()

-- | Put struct
putStructBegin :: StringType -> Put
putStructBegin _ = pure ()
putStructEnd :: Put
putStructEnd = pure ()

-- | Put field
putFieldBegin :: StringType -> ThriftType -> Word16 -> Put
putFieldBegin _ ttype ix = putType ttype
                        *> putWord16be ix
putFieldEnd :: Put
putFieldEnd = pure ()
putFieldStop :: Put
putFieldStop = putType T_STOP

-- | Put map
putMapBegin :: ThriftType -> ThriftType -> Word32 -> Put
putMapBegin k v len = putType k
                   *> putType v
                   *> putWord32be len
putMapEnd :: Put
putMapEnd = pure ()

-- | Put list
putListBegin :: ThriftType -> Word32 -> Put
putListBegin ttype len = putType ttype
                      *> putWord32be len
putListEnd :: Put
putListEnd = pure ()

-- | Put set
putSetBegin :: ThriftType -> Word32 -> Put
putSetBegin = putListBegin
putSetEnd :: Put
putSetEnd = putListEnd

-- | Put string
putString :: StringType -> Put
putString s = putWord32be (fromIntegral (B.length bs))
           *> putLazyByteString bs
  where bs = encodeUtf8 s

-- | Put ThritType
putType :: ThriftType -> Put
putType ttype = putWord8 (fromIntegral . fromEnum $ ttype)


-- * Get

-- | Get field
getField :: Binary a => Get (Word16, T.Text, a)
getField = do
    (name, _, ix) <- getFieldBegin
    (,,) <$> pure ix <*> pure name <*> (get <* getFieldEnd)

-- | Get message
getMessageBegin :: Get (StringType, MessageType, SeqId)
getMessageBegin = do
    ver <- getWord32be
    let vid = ver .&. vMask
    when (vid /= v1) $ fail "Missing version identifier"
    let mtype = (toEnum . fromIntegral) $ ver .&. 0xFF
    (,,) <$> getString <*> pure mtype <*> getWord32be
getMessageEnd :: Get ()
getMessageEnd = pure ()

-- | Get struct
getStructBegin :: Get StringType
getStructBegin = pure ""
getStructEnd :: Get ()
getStructEnd = pure ()

-- | Get field
getFieldBegin :: Get (StringType, ThriftType, Word16)
getFieldBegin = do
    ttype <- getType
    ix <- if (ttype /= T_STOP)
          then getWord16be
          else pure 0
    return $! ("", ttype, ix)
getFieldEnd :: Get ()
getFieldEnd = pure ()

-- | Get map
getMapBegin :: Get (ThriftType, ThriftType, Word32)
getMapBegin = (,,) <$> getType <*> getType <*> getWord32be
getMapEnd :: Get ()
getMapEnd = pure ()

-- | Get list
getListBegin :: Get (ThriftType, Word32)
getListBegin = (,) <$> getType <*> getWord32be
getListEnd :: Get ()
getListEnd = pure ()

-- | Get set
getSetBegin :: Get (ThriftType, Word32)
getSetBegin = getListBegin
getSetEnd :: Get ()
getSetEnd = pure ()

-- | Get string
getString :: Get T.Text
getString = do
    len <- getWord32be
    decodeUtf8 <$> getLazyByteString (fromIntegral len)

-- | Get ThriftType
getType :: Get ThriftType
getType = (toEnum . fromIntegral) <$> getWord8

-- * Miscellaneous

v1 :: Word32
v1 = 0x80010000

vMask :: Word32
vMask = 0xffff0000

third :: (a,b,c) -> c
third (_,_,x) = x

