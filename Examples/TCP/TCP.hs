import Prelude hiding (read)
import Control.WMonad.Base
import Control.WMonad.Types
import Control.WMonad.Runtime.Socket
import Data.ByteString.Char8 (pack)
import Data.Word
import Network.Simple.TCP

prog :: W ()
prog = do
    w <- read :: W Word32
    output . pack . show $ w
    prog

main :: IO ()
main = serveW HostAny "55555" prog
