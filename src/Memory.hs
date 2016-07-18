module Memory
    ( readByte
    , readHword
    , readWord
    , readDword
    ) where

import           Data.Binary.Strict.Get
import qualified Data.ByteString        as B
import           Data.Word

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
