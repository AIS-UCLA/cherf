module Client where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad (forever, liftM2, void)
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (fromStrict)
import qualified Data.List.NonEmpty as NE
import Data.X509.CertificateStore (readCertificateStore)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket.Splice (splice)
import Network.TLS
import Packet
import Punch
import System.Environment (getEnv)
import Data.Int (Int16)

client :: [String] -> IO ()
client ("ssh" : t) = ssh t
client ("attach" : t) = attach t
client ("advertise" : t) = advertise t
client _ = putStrLn "usage: cherf client <attach|advertise|ssh> ..."

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port =
  let hints = defaultHints {addrSocketType = Stream}
   in NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)

ssh :: [String] -> IO ()
ssh [host, port, remote] = withSocketsDo $ do
  addr <- resolve host port
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  fp <- B.readFile $ configDir ++ remote ++ ".sha1"
  E.bracket (open addr) close (\sock -> doHandshake host port sock >>= handle (ConnectRequest fp) sock tunnelSSH)
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
      connect sock $ addrAddress addr
      return sock
ssh _ = putStrLn "usage: cherf client ssh <addr> <port> <remote>"

attach :: [String] -> IO ()
attach [host, port, remote, remotePort] = withSocketsDo $ do
  addr <- resolve host port
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  fp <- B.readFile $ configDir ++ remote ++ ".sha1"
  E.bracket (open addr) close (\sock -> doHandshake host port sock >>= handle (ConnectRequest fp) sock (tunnelClient (read remotePort)))
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
      connect sock $ addrAddress addr
      return sock
attach _ = putStrLn "usage: cherf client attach <addr> <port> <remote> <remote port>"

advertise :: [String] -> IO ()
advertise [host, port] = withSocketsDo $ do
  addr <- resolve host port
  forever $ E.bracketOnError (open addr) close (\sock -> doHandshake host port sock >>= handle ListenRequest sock tunnelServer)
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
      connect sock $ addrAddress addr
      return sock
advertise _ = putStrLn "usage: cherf client advertise <addr> <port>"

doHandshake :: HostName -> ServiceName -> Socket -> IO Context
doHandshake serverName port sock = do
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  Just store <- readCertificateStore (configDir ++ "store")
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
      putStrLn $ "got addr: " ++ show addr
      bye ctx
      localAddr <- getSocketName sock
      close sock
      punch addr localAddr >>= tunnel
    Error code -> putStrLn $ "error: " ++ show code
    _ -> putStrLn "unimplemented"

tunnelSSH :: Socket -> IO ()
tunnelSSH sock = do
  sendAll sock $ C8.toStrict (encode (22 :: Int16))
  out <- mkSocket 1
  withFdSocket sock (sendFd out)

tunnelClient :: Int16 -> Socket -> IO ()
tunnelClient port dst = do
  sendAll dst $ C8.toStrict (encode port)
  sendAll dst $ C8.pack "deadbeef"
  void . forkIO $! forever (B.getContents >>= sendAll dst)
  void $! forever (recv dst 4096 >>= B.putStr)


tunnelServer :: Socket -> IO ()
tunnelServer src = do
  port <- (decode . fromStrict <$> recv src 2 :: IO Int16)
  addr <- resolve "localhost" (show port)
  putStrLn ("incoming connection to: " ++ show addr)
  dst <- openSocket addr
  connect dst $ addrAddress addr
  void . forkIO $! splice 1024 (src, Nothing) (dst, Nothing)
  void $! splice 1024 (dst, Nothing) (src, Nothing)
