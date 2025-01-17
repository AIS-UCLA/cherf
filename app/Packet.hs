module Packet where

import Control.Applicative (empty)
import Control.Monad (liftM2, liftM4)
import Data.Binary
import Data.ByteString (ByteString)
import Network.Socket

data ErrorCode = NoSuchFingerprint | InvalidCert deriving (Enum, Show)

data Packet
  = ConnectRequest ByteString
  | ListenRequest
  | ConnectData SockAddr
  | Error ErrorCode

{- HLINT ignore liftM8 -}
liftM8 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m r
liftM8 f m1 m2 m3 m4 m5 m6 m7 m8 = do x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; return (f x1 x2 x3 x4 x5 x6 x7 x8)

instance Binary Packet where
  put (ConnectRequest sni) = do
    put (0 :: Word8)
    put sni
  put ListenRequest = put (1 :: Word8)
  put (ConnectData (SockAddrInet port addr)) = do
    put (2 :: Word8)
    put (4 :: Word8)
    put $ toInteger port
    put $ hostAddressToTuple addr
  put (ConnectData (SockAddrInet6 port _ addr _)) = do
    put (2 :: Word8)
    put (6 :: Word8)
    put $ toInteger port
    put $ hostAddress6ToTuple addr
  put (ConnectData (SockAddrUnix _)) = mempty
  put (Error code) = do
    put (255 :: Word8)
    putWord8 $ fromInteger $ toInteger (fromEnum code) -- FIXME
  get = do
    t <- getWord8
    case t of
      0 -> ConnectRequest <$> get
      1 -> return ListenRequest
      2 -> do
        p <- getWord8
        case p of
          4 ->
            let port = fromInteger <$> get
                addr = tupleToHostAddress <$> liftM4 (,,,) get get get get
             in ConnectData <$> liftM2 SockAddrInet port addr
          6 ->
            let port = fromInteger <$> get
                addr = tupleToHostAddress6 <$> liftM8 (,,,,,,,) get get get get get get get get
             in ConnectData <$> liftM4 SockAddrInet6 port (pure 0) addr (pure 0)
          _ -> empty
      255 -> Error . toEnum . fromInteger . toInteger <$> getWord8 -- FIXME
      _ -> empty
