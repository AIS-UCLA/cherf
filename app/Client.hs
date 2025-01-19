module Client where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad (forever, liftM2, void)
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16)
import qualified Data.List.NonEmpty as NE
import Helpers
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket.Splice (splice)
import Network.TLS
import Packet
import System.Environment (getEnv)
import System.X509

client :: [String] -> IO ()
client ("ssh" : t) = attach tunnelSSH t
client ("attach" : (port : t)) = attach (tunnelClient (read port)) t
client ("advertise" : t) = advertise t
client _ = putStrLn "usage: cherf client <attach|advertise|ssh> ..."

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port =
  let hints = defaultHints {addrSocketType = Stream}
   in NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
  connect sock $ addrAddress addr
  return sock

attach :: (Socket -> IO ()) -> [String] -> IO ()
attach tunn [host, port, remote] = withSocketsDo $ do
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  fp <- B.readFile $ configDir ++ remote ++ ".sha1"
  E.bracket (resolve host port >>= open) close (\sock -> doHandshake host port sock >>= handle (ConnectRequest fp) sock tunn)
attach _ _ = putStrLn "usage: cherf client <attach port|ssh> <addr> <port> <remote>"

advertise :: [String] -> IO ()
advertise [host, port] = withSocketsDo $ do
  forever $ E.bracketOnError (resolve host port >>= open) close (\sock -> doHandshake host port sock >>= handle ListenRequest sock tunnelServer)
advertise _ = putStrLn "usage: cherf client advertise <addr> <port>"

doHandshake :: HostName -> ServiceName -> Socket -> IO Context
doHandshake serverName port sock = do
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  store <- getSystemCertificateStore
  ctx <-
    contextNew
      sock
      (defaultParamsClient serverName (C8.pack port))
        { clientHooks =
            defaultClientHooks
              { onCertificateRequest = \_ -> do
                  cred <- credentialLoadX509 (configDir ++ "cert") (configDir ++ "key")
                  case cred of
                    Left _ -> return Nothing
                    Right c -> return (Just c)
              },
          clientShared =
            defaultShared
              { sharedCAStore = store
              }
        }
  handshake ctx
  return ctx

handle :: Packet -> Socket -> (Socket -> IO ()) -> Context -> IO ()
handle pkt sock tunnel ctx = do
  sendData ctx $ encode pkt
  pkt <- decode . fromStrict <$> recvData ctx
  case pkt of
    ConnectData addr -> do
      logMesg $ "found peer: " ++ show addr
      bye ctx
      localAddr <- getSocketName sock
      close sock
      punch addr localAddr >>= tunnel
    Error code -> logMesg $ "error: " ++ show code
    _ -> logMesg "unimplemented"

tunnelSSH :: Socket -> IO ()
tunnelSSH sock = do
  sendAll sock $ C8.toStrict (encode (22 :: Int16))
  out <- mkSocket 1
  withFdSocket sock (sendFd out)

tunnelClient :: Int16 -> Socket -> IO ()
tunnelClient port dst = do
  sendAll dst $ C8.toStrict (encode port)
  void . forkIO $! forever (B.getContents >>= sendAll dst)
  void $! forever (recv dst 4096 >>= B.putStr)

tunnelServer :: Socket -> IO ()
tunnelServer src = do
  port <- (decode . fromStrict <$> recv src 2 :: IO Int16)
  addr <- resolve "localhost" (show port)
  dst <- openSocket addr
  connect dst $ addrAddress addr
  void . forkIO $! splice 1024 (src, Nothing) (dst, Nothing)
  void . forkIO $! splice 1024 (dst, Nothing) (src, Nothing)
  logMesg $ "connection established on port " ++ port ++ " from " ++ getPeerName src
