module Helpers (punch, logMesg, logMesgLn) where

import Control.Exception (catch)
import Control.Retry (constantDelay, limitRetries, retrying)
import Data.Maybe (isNothing)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.Socket

punch :: SockAddr -> SockAddr -> IO Socket
punch remote local = do
  logMesg "attempting connection "
  s <- retrying (constantDelay 100000 <> limitRetries 10) (const $ return . isNothing) (\_ -> tryPunch remote local)
  case s of
    Nothing -> error "failed punch"
    Just sock -> putStr "\n" >> return sock

tryPunch :: SockAddr -> SockAddr -> IO (Maybe Socket)
tryPunch remote local = do
  sock <- case remote of
    SockAddrInet {} -> socket AF_INET Stream defaultProtocol
    SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
    SockAddrUnix {} -> error "unreachable"
  setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
  bind sock local
  whenSupported KeepInit $ setSocketOption sock KeepInit 1
  whenSupported UserTimeout $ setSocketOption sock UserTimeout 1000
  let handler :: IOError -> IO (Maybe Socket)
      handler _ = close sock >> return Nothing
  logMesg "."
  catch (connect sock remote >> logMesg "\n" >> return (Just sock)) handler

logMesg :: String -> IO ()
logMesg m = do
  now <- getZonedTime
  let time = formatTime defaultTimeLocale "%b %e %T" now
  putStr $ "[" ++ time ++ "] " ++ m

logMesgLn :: String -> IO ()
logMesgLn m = logMesg $ m ++ "\n"
