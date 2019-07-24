{-# LANGUAGE NamedFieldPuns #-}
module N64 where

import qualified Data.ByteString     as B
import qualified Data.Vector.Unboxed as V
import           Data.Word
import Control.Monad.ST.Strict

import           Memory
import qualified VR4300

data N64 = N64
            { cpu   :: VR4300.VR4300
            , rdram :: Memory
            , memmap :: Memory
            , rom   :: B.ByteString
            }

instance ST N64 where


instance Show N64 where
    show N64 {cpu} = show cpu

newN64 :: B.ByteString -> N64
newN64 r = N64
    { cpu = VR4300.new
    , rdram = V.empty
    , memmap = V.replicate 0x20000000 0
    , rom = r
    }

run :: N64 -> N64
run = step

step :: N64 -> N64
step n64@N64 {cpu, rdram} =
    n64 {cpu = VR4300.step cpu 0x00431021}

