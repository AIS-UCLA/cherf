module Packet where
import Network.Socket
import Data.Binary
import Control.Monad (liftM2, liftM4)
import Control.Applicative (empty)
import Data.ByteString (ByteString)

data ErrorCode = NoSuchFingerprint | InvalidCert deriving Enum

data Packet =
      ConnectRequest ByteString
    | ListenRequest
    | ConnectData SockAddr
    | Error ErrorCode

instance Binary Packet where
    put (ConnectRequest sni) = do
        put (0 :: Word8)
        put sni
    put ListenRequest = put (1 :: Word8)
    put (ConnectData (SockAddrInet port addr)) = do
        put (2 :: Word8)
        put $ toInteger port
        put $ hostAddressToTuple addr
    put (ConnectData _) = mempty
    put (Error code) = do
        put (255 :: Word8)
        putWord8 $ fromInteger $ toInteger (fromEnum code) -- FIXME
    get = do t <- getWord8
             case t of
                  0 -> ConnectRequest <$> get
                  1 -> return ListenRequest
                  2 -> let port = fromInteger <$> get
                           addr = tupleToHostAddress <$> liftM4 (,,,) get get get get
                       in ConnectData <$> liftM2 SockAddrInet port addr
                  255 -> Error . toEnum . fromInteger . toInteger <$> getWord8 -- FIXME
                  _ -> empty

