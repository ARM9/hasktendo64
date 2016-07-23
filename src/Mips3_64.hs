{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mips3_64
    ( Mips3_64(..)
    , Instruction
    , step
    ) where

import           Data.Bits
import           Data.Word

import           Memory
import           Mips3     (Instruction, Mips3 (..), funct, getImm16, getRs,
                            getRt, op, regRd, regRs, regRt)

step :: (Mips3_64 p) => p -> Instruction -> p
step cpu instr = let
    fn = case op instr of
        0b000000 ->
            case funct instr of
                0b100000 -> add
                0b100001 -> addu
                0b101100 -> dadd
                0b101101 -> daddu
                _        -> unimplemented
        0b001001 -> addiu
        _        -> unimplemented
    in fn (incPc cpu) instr

class Mips3 p => Mips3_64 p where
    dadd :: p -> Instruction -> p
    dadd =
        unimplemented

    daddu :: p -> Instruction -> p
    daddu cpu instr =
        setGpr cpu rd $ rs + rt
        where
            rd = regRd instr
            rt :: Word64 = getRt cpu instr
            rs :: Word64 = getRs cpu instr
