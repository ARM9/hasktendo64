{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mips3
    ( Mips3(..), getRt, getRs, getImm16, regRd, regRs, regRt, Instruction
    , step
    , op, funct
    ) where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Text.Printf

import           Memory

type Instruction = Word32

op :: Instruction -> Instruction
op instr = instr `shiftR` 26

funct :: Instruction -> Instruction
funct = (.&.) 0b111111

regRd,regRt,regRs :: Instruction -> Instruction
regRd instr = (instr `shiftR` 11) .&. 0x1F
regRt instr = (instr `shiftR` 16) .&. 0x1F
regRs instr = (instr `shiftR` 21) .&. 0x1F

getRt, getRs :: (Mips3 p, Integral a) => p -> Instruction -> a
getRt cpu n = getGpr cpu $ regRt n
getRs cpu n = getGpr cpu $ regRs n

getImm16 :: (Integral a) => Instruction -> a
getImm16 = fromIntegral . (.&.) 0xFFFF

getShamt :: Instruction -> Instruction
getShamt instr = (instr `shiftR` 6) .&. 0x1F

getTargetAddr :: Instruction -> Address
getTargetAddr = (`shiftL` 2) . (.&.) 0x3ffffff

step :: (Mips3 p) => p -> Instruction -> p
step cpu instr = let
    fn = case op instr of
        0b000000 ->
            case funct instr of
                0b100000 -> add
                0b100001 -> addu
                _        -> unimplemented
        0b001001 -> addiu
        _        -> unimplemented
    in fn (incPc cpu) instr

class Mips3 p where
    setGpr :: (Integral a) => p -> Word32 -> a -> p
    getGpr :: (Integral a) => p -> Word32 -> a
    incPc :: p -> p

    add :: p -> Instruction -> p
    add = unimplemented

    addu :: p -> Instruction -> p
    addu cpu instr =
        setGpr cpu rd (toInt32 (rs + rt))
        where
            rd = regRd instr
            rt :: Word32 = getRt cpu instr
            rs :: Word32 = getRs cpu instr

    addiu :: p -> Instruction -> p
    addiu cpu instr =
        setGpr cpu rt (toInt32 (rs + imm))
        where
            rt = regRt instr
            rs = getRs cpu instr
            imm = toWord32 (getImm16 instr :: Int16)

    --illegal :: String -> p -> Instruction -> p
    --illegal name _ instr = error $ printf "Illegal instruction %s 0x%08X\n" name instr

    unimplemented :: p -> Instruction -> p
    unimplemented _ instr =
        error $
        printf "Unimplemented instruction 0x%08X\nop: 0b%06b\nfunct: 0b%06b\n"
        instr (op instr) (funct instr)
