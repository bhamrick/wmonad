module Control.WMonad.Wiring where

import Data.ByteString (empty)
import Control.WMonad.Base
import Control.WMonad.Types

-- This is where I'm almost entirely sure that I'm duplicating the work that Conduit did

-- Wire the outputs of the first machine into the input of the second
-- Unsafe if the first machine terminates
-- Can ensure safety with a ~ Void
connect :: W a -> W b -> W b
connect _ (WDone (b, s)) = WDone (b, empty)
connect (WDone (a, s)) _ = WDone (undefined, s)
connect w1 (WStep (Output o w2')) = WStep (Output o (connect w1 w2'))
connect (WStep (Output o w1')) (WStep (Processing g)) = connect w1' (g o)
connect (WStep (Processing f)) w2 = WStep . Processing $ \s -> connect (f s) w2
    
