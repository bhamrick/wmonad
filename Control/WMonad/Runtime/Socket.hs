module Control.WMonad.Runtime.Socket where

import Control.Monad.State
import Control.Monad.Reader
import Control.WMonad.Types
import Data.ByteString (empty)
import Network.Socket hiding (send, sendTo, recv, recvFrom, listen, accept)
import Network.Socket.ByteString
import Network.Simple.TCP

runW :: Socket -> W () -> IO ()
runW sock = fmap fst . flip runStateT empty . flip runReaderT sock . sockStack . fromW

serveW :: HostPreference -> ServiceName -> W () -> IO ()
serveW host serv w = serve host serv $ \(socket, sockaddr) -> runW socket w
