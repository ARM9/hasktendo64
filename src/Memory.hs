module Memory
    ( Memory, Address
    , readByte
    , readHword
    , readWord
    , readDword
    , toInt32, toWord32
    ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST
import           Data.Binary.Strict.Get
import           Data.Bits
import qualified Data.ByteString             as B
import           Data.Int
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word

type Memory = VM.MVector Word8
type Address = Word32

--initRam :: (PrimMonad m, VM.MVector v Word32) => m()
--initRam = VM.replicate (4*1024*1024) 0

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

-- writeWordBE :: (PrimMonad m, VM.MVector v Word32) =>
--              v (PrimState m) Word32 -> Address -> Word32 -> ST ()
writeWordBE :: (PrimMonad m) =>
                VM.MVector (PrimState m) Word32 -> Address -> Word32 -> m ()
writeWordBE mem addr val = do
    VM.write mem (fromIntegral addr) $ byte 24 val
    VM.write mem (fromIntegral $addr+1) $ byte 16 val
    VM.write mem (fromIntegral $addr+2) $ byte 8 val
    VM.write mem (fromIntegral $addr+3) $ byte 0 val
    where
        byte n x = fromIntegral $ x `shiftR` n

--writeByte :: Memory -> Address -> Word8 -> m ()
writeByte mem addr val =
    VM.write mem (fromIntegral addr) val

-- todo module this
{-# INLINE toInt32 #-}
toInt32 :: (Integral a) => a -> Int32
toInt32 = fromIntegral

{-# INLINE toWord32 #-}
toWord32 :: (Integral a) => a -> Word32
toWord32 = fromIntegral
