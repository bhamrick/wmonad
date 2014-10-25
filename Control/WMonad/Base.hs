module Control.WMonad.Base where

import Prelude hiding (drop, length, read, take)
import Control.Monad
import Control.WMonad.Types
import Data.Attoparsec.ByteString hiding (skip)
import Data.Bits
import Data.ByteString (ByteString, drop, empty, length, singleton)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Word

parseValue :: MonadConn m => Parser a -> m (Maybe a)
parseValue parser = runCont (parse parser) where
    runCont cont = process (\s ->
        case cont s of
            Fail s' _ _ -> Right (Nothing, s')
            Done s' x -> Right (Just x, s')
            Partial cont -> Left (runCont cont)
        )

class Parsable a where
    parser :: Parser a

class Parsable a => Serializable a where
    serialize :: a -> ByteString

readMaybe :: (MonadConn m, Parsable a) => m (Maybe a)
readMaybe = parseValue parser

-- Unsafe unless the parser can never fail
read :: (MonadConn m, Parsable a) => m a
read = liftM fromJust readMaybe

skip :: MonadConn m => Int -> m ()
skip n = process (\s -> let l = length s in
    if l < n
    then Left (skip (n - l))
    else Right ((), drop n s)
    )

getLine :: MonadConn m => m ByteString
getLine = do
    s <- parseValue (takeTill (== 10))
    skip 1
    return $ case s of
        Nothing -> empty
        Just x -> x

instance Parsable Word8 where
    parser = anyWord8

instance Serializable Word8 where
    serialize = singleton

instance Parsable Word16 where
    parser = do
        lo <- anyWord8
        hi <- anyWord8
        return (fromIntegral lo .|. shift (fromIntegral hi) 8)

instance Serializable Word16 where
    serialize x = let lo = fromIntegral (x .&. 0xFF) :: Word8
                      hi = fromIntegral (shift x (-8)) :: Word8
                  in serialize lo <> serialize hi

instance Parsable Word32 where
    parser = do
        lo <- parser :: Parser Word16
        hi <- parser :: Parser Word16
        return (fromIntegral lo .|. shift (fromIntegral hi) 16)

instance Serializable Word32 where
    serialize x = let lo = fromIntegral (x .&. 0xFFFF) :: Word16
                      hi = fromIntegral (shift x (-16)) :: Word16
                  in serialize lo <> serialize hi

instance Parsable Word64 where
    parser = do
        lo <- parser :: Parser Word32
        hi <- parser :: Parser Word32
        return (fromIntegral lo .|. shift (fromIntegral hi) 32)

instance Serializable Word64 where
    serialize x = let lo = fromIntegral (x .&. 0xFFFFFFFF) :: Word32
                      hi = fromIntegral (shift x (-32)) :: Word32
                  in serialize lo <> serialize hi

instance Parsable Integer where
    parser = do
        len <- parser :: Parser Word32
        bytes <- count (fromIntegral len) anyWord8
        return (foldBytes bytes)
        where
        foldBytes [] = 0
        foldBytes (b:bs) = fromIntegral b .|. shift (foldBytes bs) 8

instance Parsable ByteString where
    parser = do
        len <- parser :: Parser Word32
        take (fromIntegral len)

instance Serializable ByteString where
    serialize s = let len = fromIntegral (length s) :: Word32 in
                  serialize len <> s
