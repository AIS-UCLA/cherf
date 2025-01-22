module Helpers (punch, logMesg) where

import Control.Monad.Catch (Handler (Handler))
import Control.Retry (recovering, retryPolicyDefault)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (..))
import Network.Socket
import System.IO (hPutStr, stderr)

punch :: SockAddr -> SockAddr -> IO Socket
punch remote local = do
  logMesg $ "attempting connection to " ++ show remote
  sock <- case remote of
    SockAddrInet {} -> socket AF_INET Stream defaultProtocol
    SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
    SockAddrUnix {} -> error "unreachable"
  bind sock local
  let h :: IOError -> IO Bool
      h IOError {ioe_type = NoSuchThing} = return True
      h _ = return False
  recovering retryPolicyDefault [const $ Handler h] (\_ -> connect sock remote)
  return sock

logMesg :: String -> IO ()
logMesg m = logMesgNoLn $ m ++ "\n"

logMesgNoLn :: String -> IO ()
logMesgNoLn m = do
  now <- getZonedTime
  let time = formatTime defaultTimeLocale "%b %e %T" now
  hPutStr stderr $ "[" ++ time ++ "] " ++ m
