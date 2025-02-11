module Helpers (punch, logMesg, getConfigDir) where

import Control.Monad (liftM2)
import Control.Monad.Catch (Handler (Handler))
import Control.Retry (recovering, retryPolicyDefault)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.Socket
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError)

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

logMesg :: String -> IO ()
logMesg m = do
  time <- formatTime defaultTimeLocale "%b %e %T" <$> getZonedTime
  hPutStrLn stderr $ "[" ++ time ++ "] " ++ m
