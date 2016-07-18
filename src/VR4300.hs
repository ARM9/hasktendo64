{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VR4300 where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Text.Printf

type Instruction = Word32

data VR4300 = VR4300
            { r1  :: Word64, r2  :: Word64, r3  :: Word64, r4  :: Word64
            -- , r5  :: Word64, r6  :: Word64, r7  :: Word64, r8  :: Word64
            -- , r9  :: Word64, r10 :: Word64, r11 :: Word64, r12 :: Word64
            -- , r13 :: Word64, r14 :: Word64, r15 :: Word64, r16 :: Word64
            -- , r17 :: Word64, r18 :: Word64, r19 :: Word64, r20 :: Word64
            -- , r21 :: Word64, r22 :: Word64, r23 :: Word64, r24 :: Word64
            -- , r25 :: Word64, r26 :: Word64, r27 :: Word64, r28 :: Word64
            -- , r29 :: Word64, r30 :: Word64, r31 :: Word64
            -- , mhi :: Word64, mlo :: Word64
            , pc  :: Word64
            --, cop0 :: Cop0
            --, cop1 :: Cop1
            } deriving Show

r0 :: VR4300 -> Word64
r0 _ = 0

new :: VR4300
new = VR4300
            { r1  = 1, r2  = 1, r3  = -1, r4  = 1
            -- , r5  = 0, r6  = 0, r7  = 0, r8  = 0
            -- , r9  = 0, r10 = 0, r11 = 0, r12 = 0
            -- , r13 = 0, r14 = 0, r15 = 0, r16 = 0
            -- , r17 = 0, r18 = 0, r19 = 0, r20 = 0
            -- , r21 = 0, r22 = 0, r23 = 0, r24 = 0
            -- , r25 = 0, r26 = 0, r27 = 0, r28 = 0
            -- , r29 = 0, r30 = 0, r31 = 0
            -- , mhi = 0, mlo = 0
            , pc  = 0x80000400
            }

exec :: VR4300 -> Instruction -> VR4300
exec cpu_old instr =
    let cpu = cpu_old {pc = pc cpu_old + 4} in
    case special of
        0b000000 ->
            case op of
                0b100000 -> add cpu instr
                _        -> unimplemented $ printf "\nop: 0x%06b" op
        _        -> unimplemented $ printf "\nspecial: 0x%06b" special
    where
        special = (instr `shiftR` 24) .&. 0b111111
        op = instr .&. 0b11111111111
        unimplemented = error . (++) ("Unimplemented instruction " ++ printf "0x%08X " instr)

setGpr :: (Integral a) => VR4300 -> Word32 -> a -> VR4300
setGpr cpu rd x =
    case rd of
        0   -> cpu
        1   -> cpu {r1 = fromIntegral x}
        2   -> cpu {r2 = fromIntegral x}
        3   -> cpu {r3 = fromIntegral x}
        _   -> cpu {r4 = fromIntegral x}

getGpr :: (Integral a) => VR4300 -> Word32 -> a
getGpr cpu n =
    case n of
        0   -> 0
        1   -> fromIntegral $ r1 cpu
        2   -> fromIntegral $ r2 cpu
        3   -> fromIntegral $ r3 cpu
        _   -> fromIntegral $ r4 cpu

add :: VR4300 -> Instruction -> VR4300
add cpu instr =
    setGpr cpu rd (fromIntegral (rs + rt) :: Int32)
    where
        rd = (instr `shiftR` 11) .&. 0x1F
        rt :: Word32 = getGpr cpu $ (instr `shiftR` 16) .&. 0x1F
        rs :: Word32 = getGpr cpu $ (instr `shiftR` 21) .&. 0x1F
