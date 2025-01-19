module Server (server) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (forever, liftM2, void)
import Data.Binary (decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.X509 as X
import Data.X509.CertificateStore
import Data.X509.Validation (Fingerprint (Fingerprint), getFingerprint)
import Helpers (logMesg)
import Network.Socket
import Network.TLS
import Packet
import System.Environment (getEnv)
import System.X509

newtype ServerState = ServerState (MVar (Map.Map ByteString (SockAddr, MVar SockAddr)))

newServerState :: IO ServerState
newServerState = do
  m <- newMVar Map.empty
  return (ServerState m)

insertFingerprint :: ServerState -> Fingerprint -> SockAddr -> IO (MVar SockAddr)
insertFingerprint (ServerState m) (Fingerprint fp) addr = do
  state <- takeMVar m
  newM <- newEmptyMVar
  putMVar m $ Map.insert fp (addr, newM) state
  return newM

deleteFingerprint :: ServerState -> Fingerprint -> IO ()
deleteFingerprint (ServerState m) (Fingerprint fp) = do
  state <- takeMVar m
  putMVar m $ Map.delete fp state

lookupFingerprint :: ServerState -> Fingerprint -> IO (Maybe (SockAddr, MVar SockAddr))
lookupFingerprint (ServerState m) (Fingerprint fp) = do
  state <- takeMVar m
  putMVar m state
  return (Map.lookup fp state)

showFingerprint :: Fingerprint -> String
showFingerprint (Fingerprint fp) = (show . toLazyByteString . byteStringHex) fp

server :: [String] -> IO ()
server [port] = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = do
      state <- newServerState
      forever $
        E.bracketOnError (accept sock) (close . fst) $
          \(conn, peer) -> void $ forkFinally (handleConn state conn peer) (const $ gracefulClose conn 5000)
server _ = putStrLn "usage: cherf server <port>"

handleConn :: ServerState -> Socket -> SockAddr -> IO ()
handleConn sem sock peer = do
  configDir <- liftM2 (++) (getEnv "HOME") (pure "/.cherf/")
  Right cred <- credentialLoadX509 (configDir ++ "cert") (configDir ++ "key")
  store <- getSystemCertificateStore
  ctx <-
    contextNew
      sock
      defaultParamsServer
        { serverWantClientCert = True,
          serverShared =
            defaultShared
              { sharedCredentials = Credentials [cred],
                sharedCAStore = store
              },
          serverHooks =
            defaultServerHooks
              { onClientCertificate = \_ -> return CertificateUsageAccept
              }
        }
  handshake ctx
  pkt <- decode . fromStrict <$> recvData ctx
  process sem ctx peer pkt

process :: ServerState -> Context -> SockAddr -> Packet -> IO ()
process sem ctx peer (ConnectRequest fingerprint) = do
  remote <- lookupFingerprint sem (Fingerprint fingerprint)
  case remote of
    Just (addr, m) -> do
      logMesg $ "connect request from " ++ show peer ++ " to " ++ show addr
      let pkt = encode (ConnectData addr)
       in sendData ctx pkt
      putMVar m peer
    Nothing -> do
      logMesg $ "connect request from " ++ show peer ++ " failed (no peer)"
      let pkt = encode (Error NoSuchFingerprint)
       in sendData ctx pkt
  bye ctx
process sem ctx peer ListenRequest = do
  chain <- getClientCertificateChain ctx
  case chain of
    Just (X.CertificateChain [cert]) -> do
      let fp = getFingerprint cert X.HashSHA1
      m <- insertFingerprint sem fp peer
      logMesg $ "advertising request from " ++ show peer ++ " fp=" ++ showFingerprint fp
      remote <- takeMVar m
      sendData ctx (encode (ConnectData remote))
      bye ctx
      deleteFingerprint sem fp
    _ -> do
      putStrLn $ "advertising request from " ++ show peer ++ " failed (invalid cert)"
      sendData ctx (encode (Error InvalidCert))
      bye ctx
process _ ctx _ _ = bye ctx
