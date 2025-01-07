module Client where

import qualified Control.Exception as E
import Data.Binary (decode, encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy (fromStrict)
import qualified Data.List.NonEmpty as NE
import Data.X509.CertificateStore (readCertificateStore)
import Network.Socket
import Network.TLS
import Packet

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
  E.bracket (open addr) close (handleConn host port remote)
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
punch _ = putStrLn "usage: cherf client punch <addr> <port> <remote>"

handleConn :: HostName -> ServiceName -> String -> Socket -> IO ()
handleConn serverName port remote sock = do
  Just store <- readCertificateStore "./server.crt"
  ctx <-
    contextNew
      sock
      (defaultParamsClient "cherf.ais-ucla.org" (C8.pack port))
        { clientHooks =
            defaultClientHooks
              { onCertificateRequest = \_ -> do
                  cred <- credentialLoadX509 "./client.crt" "./client.key"
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
  fp <- B.readFile $ "./" ++ remote ++ ".sha1"
  sendData ctx $ encode (ConnectRequest fp)
  pkt <- decode . fromStrict <$> recvData ctx
  case pkt of
    ConnectData _ -> putStrLn "we got data!"
    Error code -> putStrLn $ "error: " ++ show code
    _ -> putStrLn "failed"

advertise :: [String] -> IO ()
advertise [host, port] = withSocketsDo $ do
  addr <- resolve host port
  E.bracket (open addr) close handleAdvertise
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock

handleAdvertise :: Socket -> IO ()
handleAdvertise _ = return ()
