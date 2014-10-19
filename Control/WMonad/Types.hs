{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.WMonad.Types where

import Prelude hiding (putStr, getLine, null)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.ByteString (
    ByteString,
    putStr,
    getLine,
    hGetSome,
    null,
    )
import qualified Data.ByteString as BS
import System.IO (stdin)

class Monad m => MonadConn m where
    process :: (ByteString -> Either (m a) (a, ByteString)) -> m a
    output :: ByteString -> m ()

newtype StdIOConn a = StdIOConn { asStateT :: StateT ByteString IO a }
    deriving (Functor, Monad, Applicative, MonadIO)

instance MonadState ByteString StdIOConn where
    get = StdIOConn get
    put = StdIOConn . put

instance MonadConn StdIOConn where
    process f = do
        s <- get
        if null s
        then do
            s' <- liftIO (hGetSome stdin 1024)
            case f s' of
                Left m -> put BS.empty >> m
                Right (x, s'') -> put s'' >> return x
        else do
            case f s of
                Left m -> put BS.empty >> m
                Right (x, s') -> put s' >> return x
    output = liftIO . putStr
