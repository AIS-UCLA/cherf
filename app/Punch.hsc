#ifdef FREEBSD
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif

module Punch (punch) where
import Control.Retry (recoverAll, retryPolicyDefault)
import Network.Socket

punch :: SockAddr -> SockAddr -> IO Socket
punch remoteAddr localAddr = do
  sock <- case remoteAddr of
    SockAddrInet {} -> socket AF_INET Stream defaultProtocol
    SockAddrInet6 {} -> socket AF_INET6 Stream defaultProtocol
    SockAddrUnix {} -> error "unreachable"
  bind sock localAddr
  putStrLn "setting sock opt"
#ifdef FREEBSD
  setSocketOption sock (SockOpt (#const IPPROTO_TCP) (#const TCP_KEEPINIT)) 1
#else
  setSocketOption sock UserTimeout 500
#endif
  recoverAll retryPolicyDefault (\_ -> putStrLn "trying" >> connect sock remoteAddr)
  putStrLn "success"
  return sock

