{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Control.WMonad.Types where

import Prelude hiding (putStr, getLine, null)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.ByteString (
    ByteString,
    empty,
    getLine,
    hGetSome,
    null,
    putStr,
    )
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import System.IO (stdin)

class Monad m => MonadConn m where
    process :: (ByteString -> Either (m a) (a, ByteString)) -> m a
    output :: ByteString -> m ()

    buffer :: ByteString -> m ()
    buffer s = process (\s' -> Right ((), s <> s'))

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
    buffer s = modify (s <>)

-- Suspension functor for the W monad
data WF a = Processing (ByteString -> a)
          | Output ByteString a

instance Functor WF where
    fmap f (Processing g) = Processing (f . g)
    fmap f (Output o x) = Output o (f x)

-- The W Monad is the initial MonadConn
data W a = WDone (a, ByteString)
         | WStep (WF (W a))

instance Monad W where
    return x = WDone (x, empty)
    WDone (x, s) >>= f = buffer' s (f x) where
        buffer' s (WDone (x, s')) = WDone (x, s <> s')
        buffer' s (WStep (Output o w)) = WStep (Output o (buffer' s w))
        buffer' s (WStep (Processing g)) = if null s then WStep (Processing g) else g s
    WStep wf >>= f = case wf of
        Output o w -> WStep $ Output o (w >>= f)
        Processing g -> WStep . Processing $ (\s -> g s >>= f)

instance Functor W where
    fmap = liftM

instance Applicative W where
    pure = return
    (<*>) = ap

instance MonadConn W where
    process f = WStep . Processing $ either id WDone . f
    output x = WStep . Output x $ return ()

fromW :: MonadConn m => W a -> m a
fromW (WDone (x, s)) = buffer s >> return x
fromW (WStep (Processing f)) = process (Left . fromW . f)
fromW (WStep (Output o x)) = output o >> fromW x
