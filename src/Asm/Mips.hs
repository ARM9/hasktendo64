{-# LANGUAGE BinaryLiterals #-}
module Asm.Mips where

import           Data.Bits
import           Data.Word

opcode :: Word32 -> Word32 -> Word32
opcode op funct = (op `shiftL` 26) .|. funct

op_3reg :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_3reg op rs rt rd funct =
    opcode op funct .|. fields3
    where
        fields3 = (rs `shiftL` 21) .|. (rt `shiftL` 16) .|. (rd `shiftL` 11)

op_2reg_imm :: Word32 -> Word32 -> Word32 -> Word16 -> Word32
op_2reg_imm op rs rt imm =
    opcode op 0 .|. fields2 .|. fromIntegral imm
    where
        fields2 = (rs `shiftL` 21) .|. (rt `shiftL` 16)

add rd rs rt    = op_3reg        0b000000 rs rt rd 0b100000
addu rd rs rt   = op_3reg        0b000000 rs rt rd 0b100001
addi rt rs imm  = op_2reg_imm    0b001000 rs rt imm
addiu rt rs imm = op_2reg_imm    0b001001 rs rt imm
dadd rd rs rt   = op_3reg        0b000000 rs rt rd 0b101100
daddu rd rs rt  = op_3reg        0b000000 rs rt rd 0b101101
