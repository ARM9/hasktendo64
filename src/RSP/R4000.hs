module RSP.R4000 where

import           Data.Word

data R4000 = R4000
            { r1  :: Word32, r2  :: Word32, r3  :: Word32, r4  :: Word32
            , r5  :: Word32, r6  :: Word32, r7  :: Word32, r8  :: Word32
            , r9  :: Word32, r10 :: Word32, r11 :: Word32, r12 :: Word32
            , r13 :: Word32, r14 :: Word32, r15 :: Word32, r16 :: Word32
            , r17 :: Word32, r18 :: Word32, r19 :: Word32, r20 :: Word32
            , r21 :: Word32, r22 :: Word32, r23 :: Word32, r24 :: Word32
            , r25 :: Word32, r26 :: Word32, r27 :: Word32, r28 :: Word32
            , r29 :: Word32, r30 :: Word32, r31 :: Word32
            , mhi :: Word32, mlo :: Word32
            , pc  :: Word32
            --, cop0 :: Cop0
            --, cop2 :: RSP.VPU
            } deriving Show
