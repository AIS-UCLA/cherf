module Client where

import Control.Concurrent (forkFinally, forkIO, newQSem, signalQSem, waitQSem)
import qualified Control.Exception as E
import Control.Monad (forever, void)
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
import System.X509

client :: [String] -> IO ()
client ("ssh" : t) = attach tunnelSSH t
client ("attach" : (port : t)) = attach (tunnelClient (read port)) t
client ("advertise" : t) = advertise t
client _ = putStrLn "usage: cherf client <attach|advertise|ssh> ..."

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = NE.head <$> getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0}) -- Allows us to immediately switch from signaling server to remote
  connect sock $ addrAddress addr
  return sock

attach :: (Socket -> IO ()) -> [String] -> IO ()
attach tunn [host, port, remote] = withSocketsDo $ do
  configDir <- getConfigDir
  fp <- B.readFile $ configDir ++ remote ++ ".sha1" -- Read the raw SHA1 fingerprint of our desired target
  E.bracket (resolve host port >>= open) close (\sock -> doHandshake host port sock >>= handle (ConnectRequest fp) sock tunn)
attach _ _ = putStrLn "usage: cherf client <attach port|ssh> <addr> <port> <remote>"

advertise :: [String] -> IO ()
advertise [host, port] = withSocketsDo $ do
  qs <- newQSem 4
  forever $ waitQSem qs >> forkFinally (E.bracketOnError (resolve host port >>= open) (\s -> close s >> logMesg "error occured, retrying") (\sock -> doHandshake host port sock >>= handle ListenRequest sock tunnelServer)) (const $ signalQSem qs)
advertise _ = putStrLn "usage: cherf client advertise <addr> <port>"

doHandshake :: HostName -> ServiceName -> Socket -> IO Context
doHandshake serverName port sock = do
  configDir <- getConfigDir
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
  logMesg "connection initiated, passing fd"
  out <- mkSocket 1
  withFdSocket sock (sendFd out) -- ProxyUseFdpass (TODO: Windows support)

tunnelClient :: Int16 -> Socket -> IO ()
tunnelClient port dst = do
  sendAll dst $ C8.toStrict (encode port)
  void . forkIO $! forever (C8.getLine >>= sendAll dst)
  void $! forever (recv dst 4096 >>= B.putStr)

tunnelServer :: Socket -> IO ()
tunnelServer src = do
  port <- show <$> (decode . fromStrict <$> recv src 2 :: IO Int16) -- TODO: whitelist ports?
  addr <- resolve "localhost" port
  name <- show <$> getPeerName src
  logMesg $ "received request port=" ++ port ++ " from " ++ show name
  void $
    forkIO $!
      E.handle ((\_ -> return ()) :: IOError -> IO ()) $
        E.bracket
          (openSocket addr)
          (\sock -> close sock >> logMesg ("connection closed on port " ++ port ++ " from " ++ name))
          ( \dst -> do
              connect dst $ addrAddress addr
              logMesg $ "connection established on port " ++ port ++ " from " ++ name
              void . forkIO $! forever (recv src 4096 >>= sendAll dst)
              void $! forever (recv dst 4096 >>= sendAll src)
              -- TODO: use splice
              -- void . forkIO $! splice 1024 (src, Nothing) (dst, Nothing)
              -- splice 1024 (dst, Nothing) (src, Nothing)
          )
