{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VR4300 (VR4300(..), Instruction, new, step, run) where

import           Data.Word

import qualified Mips3
import           Mips3_64

data VR4300 = VR4300
            { r1  :: Word64, r2  :: Word64, r3  :: Word64, r4  :: Word64
            , r5  :: Word64, r6  :: Word64, r7  :: Word64, r8  :: Word64
            , r9  :: Word64, r10 :: Word64, r11 :: Word64, r12 :: Word64
            , r13 :: Word64, r14 :: Word64, r15 :: Word64, r16 :: Word64
            , r17 :: Word64, r18 :: Word64, r19 :: Word64, r20 :: Word64
            , r21 :: Word64, r22 :: Word64, r23 :: Word64, r24 :: Word64
            , r25 :: Word64, r26 :: Word64, r27 :: Word64, r28 :: Word64
            , r29 :: Word64, r30 :: Word64, r31 :: Word64
            , mhi :: Word64, mlo :: Word64
            , pc  :: Word64
            --, cop0 :: Cop0
            --, cop1 :: Cop1
            } deriving Show

new :: VR4300
new = VR4300
            { r1  = 0, r2  = 0, r3  = 0, r4  = 0
            , r5  = 0, r6  = 0, r7  = 0, r8  = 0
            , r9  = 0, r10 = 0, r11 = 0, r12 = 0
            , r13 = 0, r14 = 0, r15 = 0, r16 = 0
            , r17 = 0, r18 = 0, r19 = 0, r20 = 0
            , r21 = 0, r22 = 0, r23 = 0, r24 = 0
            , r25 = 0, r26 = 0, r27 = 0, r28 = 0
            , r29 = 0, r30 = 0, r31 = 0
            , mhi = 0, mlo = 0
            , pc  = 0x80000400
            }

instance Mips3.Mips3 VR4300 where
    setGpr cpu rd x =
        case rd of
            0   -> cpu
            1   -> cpu {r1 = fromIntegral x}
            2   -> cpu {r2 = fromIntegral x}
            3   -> cpu {r3 = fromIntegral x}
            4   -> cpu {r4 = fromIntegral x}
            5   -> cpu {r5 = fromIntegral x}
            6   -> cpu {r6 = fromIntegral x}
            7   -> cpu {r7 = fromIntegral x}
            8   -> cpu {r8 = fromIntegral x}
            9   -> cpu {r9 = fromIntegral x}
            10  -> cpu {r10 = fromIntegral x}
            11  -> cpu {r11 = fromIntegral x}
            12  -> cpu {r12 = fromIntegral x}
            13  -> cpu {r13 = fromIntegral x}
            14  -> cpu {r14 = fromIntegral x}
            15  -> cpu {r15 = fromIntegral x}
            16  -> cpu {r16 = fromIntegral x}
            17  -> cpu {r17 = fromIntegral x}
            18  -> cpu {r18 = fromIntegral x}
            19  -> cpu {r19 = fromIntegral x}
            20  -> cpu {r20 = fromIntegral x}
            21  -> cpu {r21 = fromIntegral x}
            22  -> cpu {r22 = fromIntegral x}
            23  -> cpu {r23 = fromIntegral x}
            24  -> cpu {r24 = fromIntegral x}
            25  -> cpu {r25 = fromIntegral x}
            26  -> cpu {r26 = fromIntegral x}
            27  -> cpu {r27 = fromIntegral x}
            28  -> cpu {r28 = fromIntegral x}
            29  -> cpu {r29 = fromIntegral x}
            30  -> cpu {r30 = fromIntegral x}
            31  -> cpu {r31 = fromIntegral x}
            _   -> error "Invalid register"

    getGpr cpu n =
        case n of
            0   -> 0
            1   -> fromIntegral $ r1 cpu
            2   -> fromIntegral $ r2 cpu
            3   -> fromIntegral $ r3 cpu
            4   -> fromIntegral $ r4 cpu
            5   -> fromIntegral $ r5 cpu
            6   -> fromIntegral $ r6 cpu
            7   -> fromIntegral $ r7 cpu
            8   -> fromIntegral $ r8 cpu
            9   -> fromIntegral $ r9 cpu
            10  -> fromIntegral $ r10 cpu
            11  -> fromIntegral $ r11 cpu
            12  -> fromIntegral $ r12 cpu
            13  -> fromIntegral $ r13 cpu
            14  -> fromIntegral $ r14 cpu
            15  -> fromIntegral $ r15 cpu
            16  -> fromIntegral $ r16 cpu
            17  -> fromIntegral $ r17 cpu
            18  -> fromIntegral $ r18 cpu
            19  -> fromIntegral $ r19 cpu
            20  -> fromIntegral $ r20 cpu
            21  -> fromIntegral $ r21 cpu
            22  -> fromIntegral $ r22 cpu
            23  -> fromIntegral $ r23 cpu
            24  -> fromIntegral $ r24 cpu
            25  -> fromIntegral $ r25 cpu
            26  -> fromIntegral $ r26 cpu
            27  -> fromIntegral $ r27 cpu
            28  -> fromIntegral $ r28 cpu
            29  -> fromIntegral $ r29 cpu
            30  -> fromIntegral $ r30 cpu
            31  -> fromIntegral $ r31 cpu
            _   -> error "Invalid register"

    incPc cpu@VR4300 {pc} = cpu {pc = pc + 4}

instance Mips3_64 VR4300 where

-- | Execute a list of instructions
run :: VR4300 -> [Instruction] -> [VR4300]
run cpu []      = [cpu]
run cpu (x:xs)  = cpu : run (step cpu x) xs
