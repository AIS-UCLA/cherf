module Packet where

import Control.Applicative (empty)
import Control.Monad (liftM2, liftM4, replicateM)
import Data.Binary
import Data.ByteString (ByteString)
import Network.Socket

data ErrorCode = NoSuchFingerprint | InvalidCert deriving (Enum, Show)

data CherfOption = BirthdayMode deriving (Enum)

instance Binary CherfOption where
  put BirthdayMode = putWord8 1
  get = toEnum . fromIntegral <$> getWord8

data Packet
  = ConnectRequest ByteString
  | ListenRequest
  | ConnectData SockAddr [CherfOption]
  | ConnectOption CherfOption
  | Error ErrorCode

instance Binary Packet where
  put (ConnectRequest fp) = do
    put (0 :: Word8)
    put fp
  put ListenRequest = put (1 :: Word8)
  put (ConnectData (SockAddrInet port addr) []) = do
    put (2 :: Word8)
    put (4 :: Word8)
    put $ toInteger port
    put $ hostAddressToTuple addr
  put (ConnectData (SockAddrInet port addr) opts) = do
    put (2 :: Word8)
    put (5 :: Word8)
    put $ toInteger port
    put $ hostAddressToTuple addr
    put opts
  put (ConnectData (SockAddrInet6 port _ addr _) []) = do
    put (2 :: Word8)
    put (6 :: Word8)
    put $ toInteger port
    put $ hostAddress6ToTuple addr
  put (ConnectData _ _) = mempty
  put (ConnectOption opt) = do
    put (3 :: Word8)
    putWord8 $ (fromIntegral . fromEnum) opt
  put (Error code) = do
    put (255 :: Word8)
    putWord8 $ (fromIntegral . fromEnum) code
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
             in liftM2 ConnectData (liftM2 SockAddrInet port addr) (pure [])
          5 ->
            let port = fromInteger <$> get
                addr = tupleToHostAddress <$> liftM4 (,,,) get get get get
             in liftM2 ConnectData (liftM2 SockAddrInet port addr) get
          6 ->
            let port = fromInteger <$> get
                addr = tupleToHostAddress6 . (\[a, b, c, d, e, f, g, h] -> (a, b, c, d, e, f, g, h)) <$> replicateM 8 get
             in liftM2 ConnectData (liftM4 SockAddrInet6 port (pure 0) addr (pure 0)) (pure [])
          _ -> empty
      3 -> ConnectOption . toEnum . fromIntegral <$> getWord8
      255 -> Error . toEnum . fromIntegral <$> getWord8
      _ -> empty
