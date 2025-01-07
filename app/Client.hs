module Client where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad (forever, liftM2, void)
import Control.Retry (recoverAll, retryPolicyDefault)
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (fromStrict)
import qualified Data.List.NonEmpty as NE
import Data.X509.CertificateStore (readCertificateStore)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Network.TLS
import Packet
import System.Environment (getEnv)

client :: [String] -> IO ()
client ("punch" : t) = punch t
client ("advertise" : t) = advertise t
client _ = putStrLn "usage: cherf client <punch|advertise> ..."

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port =
  let hints = defaultHints {addrSocketType = Stream}
   in NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)

punch :: [String] -> IO ()
punch [host, port, remote] = withSocketsDo $ do
  addr <- resolve host port
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  fp <- B.readFile $ configDir ++ remote ++ ".sha1"
  E.bracket (open addr) close (\sock -> doHandshake host port sock >>= handlePunch (ConnectRequest fp) sock)
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
      connect sock $ addrAddress addr
      return sock
punch _ = putStrLn "usage: cherf client punch <addr> <port> <remote>"

advertise :: [String] -> IO ()
advertise [host, port] = withSocketsDo $ do
  addr <- resolve host port
  forever $ E.bracketOnError (open addr) close (\sock -> void $ forkIO (doHandshake host port sock >>= handlePunch ListenRequest sock))
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

handlePunch :: Packet -> Socket -> Context -> IO ()
handlePunch pkt sock ctx = do
  sendData ctx $ encode pkt
  pkt <- decode . fromStrict <$> recvData ctx
  case pkt of
    ConnectData addr -> do
      putStrLn $ "got addr: " ++ show addr
      bye ctx
      localAddr <- getSocketName sock
      close sock
      sock <- case addr of
        SockAddrInet {} -> socket AF_INET Stream defaultProtocol
        SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
      bind sock localAddr
      recoverAll retryPolicyDefault (\_ -> connect sock addr)
      void $ send sock $ C8.pack "hello"
      tmp <- recv sock 10
      print tmp
    Error code -> putStrLn $ "error: " ++ show code
    _ -> putStrLn "unimplemented"
