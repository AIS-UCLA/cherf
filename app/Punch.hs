module Punch (punch) where

import Control.Exception (catch)
import Control.Retry (constantDelay, limitRetries, retrying)
import Data.Maybe (isNothing)
import Network.Socket
import System.IO.Error (IOError)

punch :: SockAddr -> SockAddr -> IO Socket
punch remote local = do
  s <- retrying (constantDelay 100000 <> limitRetries 10) (const $ return . isNothing) (\_ -> tryPunch remote local)
  case s of
    Nothing -> error "failed punch"
    Just sock -> putStrLn "success" >> return sock

tryPunch :: SockAddr -> SockAddr -> IO (Maybe Socket)
tryPunch remote local = do
  sock <- case remote of
    SockAddrInet {} -> socket AF_INET Stream defaultProtocol
    SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
    SockAddrUnix {} -> error "unreachable"
  setSockOptValue sock Linger $ SockOptValue (StructLinger {sl_onoff = 1, sl_linger = 0})
  bind sock local
  putStrLn "setting sock opt"
  whenSupported KeepInit $ setSocketOption sock KeepInit 1
  whenSupported UserTimeout $ setSocketOption sock UserTimeout 1000
  let handler :: IOError -> IO (Maybe Socket)
      handler _ = close sock >> return Nothing
  catch (connect sock remote >> return (Just sock)) handler
