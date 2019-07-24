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

bcf :: Instruction -> Instruction
bcf instr = (instr `shiftR` 16) .&. 0x3FF

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

getOffset16 :: (Integral a) => Instruction -> a
getOffset16 instr = fromIntegral $ (instr .&. 0xFFFF) `shiftL` 2

getTargetAddr :: Instruction -> Address
getTargetAddr = (`shiftL` 2) . (.&.) 0x3ffffff

step :: (Mips3 p) => p -> Instruction -> p
step cpu instr = let
    fn = case op instr of
        0b000000 ->
            case funct instr of
                0b100000 -> add
                0b100001 -> addu
                0b100100 -> and
                0b101100 -> dadd
                0b101101 -> daddu
                _        -> unimplemented
        0b001000 -> addi
        0b001001 -> addiu
        0b001100 -> andi
        0b010001 ->
            case bcf instr of
                0b01000_00000 -> bc1f
        _        -> unimplemented
    in fn (incPc cpu) instr

class Mips3 p where
    setGpr :: (Integral a) => p -> Word64 -> a -> p
    getGpr :: (Integral a) => p -> Word64 -> a

    setPc :: (Integral a) => p -> Word32 -> a -> p
    incPc :: p -> p

    add :: p -> Instruction -> p
    add cpu instr =
        setGpr cpu rd (toInt64 (rs + rt))
        where
            rd = regRd instr
            rt :: Word32 = getRt cpu instr
            rs :: Word32 = getRs cpu instr

    addu :: p -> Instruction -> p
    addu cpu instr =
        setGpr cpu rd (toInt64 (rs + rt))
        where
            rd = regRd instr
            rt :: Word32 = getRt cpu instr
            rs :: Word32 = getRs cpu instr

    addiu :: p -> Instruction -> p
    addiu cpu instr =
        setGpr cpu rt (toInt64 (rs + imm))
        where
            rt = regRt instr
            rs = getRs cpu instr
            imm = toInt32 (getImm16 instr :: Int16)

    and :: p -> Instruction -> p
    and cpu instr =
        setGpr cpu rd (toWord64 (rs .&. rt))
        where
            rd = regRd instr
            rt :: Word32 = getRt cpu instr
            rs :: Word32 = getRs cpu instr

    andi :: p -> Instruction -> p
    andi cpu instr =
        setGpr cpu rt (toWord64 (rs .&. imm))
        where
            rt = regRt instr
            rs = getRs cpu instr
            imm = toWord32 (getImm16 instr :: Word16)
{-
    bc1f :: p -> Instruction -> p
    bc1f cpu instr =
        if coc
            setPc cpu $ ds_pc + offset
            setBranchTaken cpu ds_pc
        where
            coc :: Boolean = coc . cop1 cpu
            ds_pc :: Int32 = pc cpu
            offset :: Int32 = getOffset16 ds_pc
-}
    dadd :: p -> Instruction -> p
    dadd =
        setGpr cpu rd $ rs + rt
        -- trap on overflow
        where
            rd = regRd instr
            rt :: Word64 = getRt cpu instr
            rs :: Word64 = getRs cpu instr

    daddu :: p -> Instruction -> p
    daddu cpu instr =
        setGpr cpu rd $ rs + rt
        where
            rd = regRd instr
            rt :: Word64 = getRt cpu instr
            rs :: Word64 = getRs cpu instr

    --illegal :: String -> p -> Instruction -> p
    --illegal name _ instr = error $ printf "Illegal instruction %s 0x%08X\n" name instr

    unimplemented :: p -> Instruction -> p
    unimplemented _ instr =
        error $
        printf "Unimplemented instruction 0x%08X\nop: 0b%06b\nfunct: 0b%06b\n"
        instr (op instr) (funct instr)

