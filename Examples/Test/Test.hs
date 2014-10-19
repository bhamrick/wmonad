import Prelude hiding (getLine, null, tail, take, read)
import Control.Monad.State
import Control.WMonad.Base
import Control.WMonad.Types
import Data.Attoparsec.ByteString hiding (skip)
import Data.ByteString 
    ( ByteString
    , breakByte
    , empty
    , tail
    , null
    )
import Data.ByteString.Char8 (pack)
import Data.Functor
import Data.Word
import System.IO (hFlush, stdout)

prog :: StdIOConn ()
prog = do
    s <- getLine
    output . pack . show $ s
    liftIO $ hFlush stdout
    prog

prog2 :: StdIOConn ()
prog2 = do
    w <- read :: StdIOConn Word32
    output . pack . show $ w
    liftIO $ hFlush stdout
    prog2

runStdIOConn :: StdIOConn () -> IO ()
runStdIOConn p = fst <$> runStateT (asStateT p) empty

main :: IO ()
main = runStdIOConn prog2
