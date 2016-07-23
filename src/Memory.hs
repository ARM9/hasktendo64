module Memory
    ( Memory, Address
    , readByte
    , readHword
    , readWord
    , readDword
    , toInt32, toWord32
    ) where

import           Data.Binary.Strict.Get
import qualified Data.ByteString        as B
import           Data.Int
import qualified Data.Vector            as V
import           Data.Word

type Memory = V.Vector Word32
type Address = Word32

readByte :: B.ByteString -> Word8
readByte mem = either error id g
    where (g,_) = runGet getWord8 mem

readHword :: B.ByteString -> Word16
readHword mem = either error id g
    where (g,_) = runGet getWord16be mem

readWord :: B.ByteString -> Word32
readWord mem = either error id g
    where (g,_) = runGet getWord32be mem

readDword :: B.ByteString -> Word64
readDword mem = either error id g
    where (g,_) = runGet getWord64be mem

writeWord :: Memory -> Address -> Memory
writeWord mem addr = mem

-- todo module this
toInt32 :: (Integral a) => a -> Int32
toInt32 = fromIntegral

toWord32 :: (Integral a) => a -> Word32
toWord32 = fromIntegral
