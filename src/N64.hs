{-# LANGUAGE NamedFieldPuns #-}
module N64 where

import qualified Data.ByteString as B
import           Data.Word
import qualified VR4300

data N64 = N64
            { cpu :: VR4300.VR4300
            , ram :: B.ByteString
            , rom :: B.ByteString
            }

newN64 :: B.ByteString -> N64
newN64 r = N64
  { cpu = VR4300.new
  , ram = B.empty
  , rom = r
  }

step :: N64 -> N64
step n64@N64 {cpu} = n64 {cpu = VR4300.exec cpu 0x00431020}
