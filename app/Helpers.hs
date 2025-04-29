module Helpers (bdayLocal, bdayRemote, punch, logMesg, getConfigDir) where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent.Async
import Control.Monad (liftM2)
import Control.Monad.Catch (Handler (Handler))
import Control.Retry (recovering, retryPolicyDefault)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.Socket
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError)
import System.Random (getStdRandom, uniformShuffleList)

getConfigDir :: IO String
getConfigDir = liftM2 (++) (getEnv "HOME") (pure "/.cherf/") -- TODO: fix on Windows

punch :: SockAddr -> SockAddr -> IO Socket
punch remote local =
  logMesg ("attempting connection to " ++ show remote)
    >> recovering
      retryPolicyDefault
      [const $ Handler $ return . isDoesNotExistError, const $ Handler $ return . isAlreadyInUseError]
      ( const $ do
          sock <- case remote of
            SockAddrInet {} -> socket AF_INET Stream defaultProtocol
            SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
            SockAddrUnix {} -> error "unreachable"
          bind sock local
          connect sock remote
          return sock
      )

genPorts :: Int -> IO [PortNumber]
genPorts n = take n <$> getStdRandom (uniformShuffleList [1024 .. 65535])

bdayRemote :: SockAddr -> SockAddr -> IO Socket
bdayRemote (SockAddrInet _ addr) local = do
  ports <- genPorts 1024
  runConcurrently $
    foldr1 (<|>) [Concurrently (go i) | i <- ports]
  where
    go port = punch (SockAddrInet port addr) local

bdayLocal :: SockAddr -> SockAddr -> IO Socket
bdayLocal remote (SockAddrInet _ addr) = do
  ports <- genPorts 1024
  runConcurrently $
    foldr1 (<|>) [Concurrently (go i) | i <- ports]
  where
    go port = punch remote (SockAddrInet port addr)

logMesg :: String -> IO ()
logMesg m = do
  time <- formatTime defaultTimeLocale "%b %e %T" <$> getZonedTime
  hPutStrLn stderr $ "[" ++ time ++ "] " ++ m
