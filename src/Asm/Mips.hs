{-# LANGUAGE BinaryLiterals #-}
module Asm.Mips where

import           Data.Bits
import           Data.Word
import           Text.Printf

-- | Return an integer with the n least significant bits set
bits :: (Integral a, Integral b, Bits b) => a -> b
bits n = 1 `shiftL` fromIntegral n - 1

-- | Mask the n least significant bits of x
maskBits :: (Integral a, Integral b, Bits b) => a -> b -> b
maskBits n x = bits n .&. x

-- | Combine op and function field for TODO DIAGRAM opcodes
opcode :: Word32 -> Word32 -> Word32
opcode op funct = (op `shiftL` 26) .|. funct

op_3reg :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_3reg op rs rt rd funct =
    opcode op funct .|. fields
    where
        fields = (rs `shiftL` 21) .|. (rt `shiftL` 16) .|. (rd `shiftL` 11)

op_2reg :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg op rs rt funct =
    op_3reg op rs rt 0 funct

op_1reg :: Word32 -> Word32 -> Word32 -> Word32
op_1reg op rs funct =
    op_3reg op rs 0 0 funct

op_2reg_rsrd :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_rsrd op rs rd funct =
    opcode op funct .|. fields
    where
        fields = (rs `shiftL` 21) .|. (rd `shiftL` 11)

op_2reg_imm :: Word32 -> Word32 -> Word32 -> Word16 -> Word32
op_2reg_imm op rs rt imm =
    opcode op 0 .|. fields .|. fromIntegral imm
    where
        fields = (rs `shiftL` 21) .|. (rt `shiftL` 16)

op_2reg_offs :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_offs op rs rt imm =
    opcode op 0 .|. fields .|. ((imm `shiftR` 2) .&. 0xFFFF)
    where
        fields = (rs `shiftL` 21) .|. (rt `shiftL` 16)

op_2reg_sa :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_sa op rt rd sa funct =
    opcode op funct .|. fields
    where
        fields = (rt `shiftL` 16) .|. (rd `shiftL` 11) .|. (sa `shiftL` 6)

-- | branch coprocessor Z condition
op_bcopzc :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_bcopzc op cp subop cond offs =
    (op `shiftL` 28) .|. (cp `shiftL` 26) .|. (subop `shiftL` 21) .|. (cond `shiftL` 16) .|. ((offs `shiftR` 2) .&. 0xFFFF)

op_copz_mov :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_copz_mov op cp subop rt rd =
    (op `shiftL` 28) .|. (cp `shiftL` 26) .|. (subop `shiftL` 21) .|. (rt `shiftL` 16) .|. (rd `shiftL` 11)

op_copz :: Word32 -> Word32 -> Word32 -> Word32
op_copz op cp cofun =
    (op `shiftL` 28) .|. (cp `shiftL` 26) .|. (1 `shiftL` 25) .|. maskBits 25 cofun

op_jump :: Word32 -> Word32 -> Word32
op_jump op target = (op `shiftL` 26) .|. maskBits 26 (target `shiftR` 2)

add rd rs rt        = op_3reg       0b000000 rs rt rd 0b100000
addi rt rs imm      = op_2reg_imm   0b001000 rs rt imm
addiu rt rs imm     = op_2reg_imm   0b001001 rs rt imm
addu rd rs rt       = op_3reg       0b000000 rs rt rd 0b100001
and rd rs rt        = op_3reg       0b000000 rs rt rd 0b100100
andi rt rs imm      = op_2reg_imm   0b001100 rs rt imm
bc0f                = op_bcopzc     0b0100 0 0b01000  0b00000
bc0fl               = op_bcopzc     0b0100 0 0b01000  0b00010
bc0t                = op_bcopzc     0b0100 0 0b01000  0b00001
bc0tl               = op_bcopzc     0b0100 0 0b01000  0b00011
bc1f                = op_bcopzc     0b0100 1 0b01000  0b00000
bc1fl               = op_bcopzc     0b0100 1 0b01000  0b00010
bc1t                = op_bcopzc     0b0100 1 0b01000  0b00001
bc1tl               = op_bcopzc     0b0100 1 0b01000  0b00011
bc2f                = op_bcopzc     0b0100 2 0b01000  0b00000
bc2fl               = op_bcopzc     0b0100 2 0b01000  0b00010
bc2t                = op_bcopzc     0b0100 2 0b01000  0b00001
bc2tl               = op_bcopzc     0b0100 2 0b01000  0b00011
bc3f                = op_bcopzc     0b0100 3 0b01000  0b00000
bc3fl               = op_bcopzc     0b0100 3 0b01000  0b00010
bc3t                = op_bcopzc     0b0100 3 0b01000  0b00001
bc3tl               = op_bcopzc     0b0100 3 0b01000  0b00011
beq rs rt offs      = op_2reg_offs  0b000100 rs rt offs
beql rs rt offs     = op_2reg_offs  0b010100 rs rt offs
bgez rs offs        = op_2reg_offs  0b000001 rs 0b00001 offs
bgezal rs offs      = op_2reg_offs  0b000001 rs 0b10001 offs
bgezall rs offs     = op_2reg_offs  0b000001 rs 0b10011 offs
bgezl rs offs       = op_2reg_offs  0b000001 rs 0b00011 offs
bgtz rs offs        = op_2reg_offs  0b000111 rs 0b00000 offs
bgtzl rs offs       = op_2reg_offs  0b010111 rs 0b00000 offs
blez rs offs        = op_2reg_offs  0b000110 rs 0b00000 offs
blezl rs offs       = op_2reg_offs  0b010110 rs 0b00000 offs
bltz rs offs        = op_2reg_offs  0b000001 rs 0b00000 offs
bltzal rs offs      = op_2reg_offs  0b000001 rs 0b10000 offs
bltzall rs offs     = op_2reg_offs  0b000001 rs 0b10010 offs
bltzl rs offs       = op_2reg_offs  0b000001 rs 0b00010 offs
bne rs rt offs      = op_2reg_offs  0b000101 rs rt offs
bnel rs rt offs     = op_2reg_offs  0b010101 rs rt offs
break code          = ((code .&. (1 `shiftL` 20 - 1)) `shiftL` 6) .|. 0b001101
cache op base imm   = op_2reg_imm   0b101111 base op imm
cfc1 rt rd          = op_copz_mov   0b0100 1 0b00010 rt rd
cfc2 rt rd          = op_copz_mov   0b0100 2 0b00010 rt rd
cfc3 rt rd          = op_copz_mov   0b0100 3 0b00010 rt rd
cop0 cofun          = op_copz       0b0100 0 cofun
cop1 cofun          = op_copz       0b0100 1 cofun
cop2 cofun          = op_copz       0b0100 2 cofun
cop3 cofun          = op_copz       0b0100 3 cofun
ctc1 rt rd          = op_copz_mov   0b0100 1 0b00110 rt rd
ctc2 rt rd          = op_copz_mov   0b0100 2 0b00110 rt rd
ctc3 rt rd          = op_copz_mov   0b0100 3 0b00110 rt rd
dadd rd rs rt       = op_3reg       0b000000 rs rt rd 0b101100
daddi rt rs imm     = op_2reg_imm   0b011000 rs rt imm
daddiu rt rs imm    = op_2reg_imm   0b011001 rs rt imm
daddu rd rs rt      = op_3reg       0b000000 rs rt rd 0b101101
ddiv rs rt          = op_2reg       0b000000 rs rt    0b011110
ddivu rs rt         = op_2reg       0b000000 rs rt    0b011111
div rs rt           = op_2reg       0b000000 rs rt    0b011010
divu rs rt          = op_2reg       0b000000 rs rt    0b011011
dmfc0 rt rd         = op_copz_mov   0b0100 0 0b00001 rt rd
dmtc0 rt rd         = op_copz_mov   0b0100 0 0b00101 rt rd
dmult rs rt         = op_2reg       0b000000 rs rt    0b011100
dmultu rs rt        = op_2reg       0b000000 rs rt    0b011101
dsll rd rt sa       = op_2reg_sa    0b000000 rt rd sa 0b111000
dsllv rd rt rs      = op_3reg       0b000000 rs rt rd 0b010100
dsll32 rd rt sa     = op_2reg_sa    0b000000 rt rd sa 0b111100
dsra rd rt sa       = op_2reg_sa    0b000000 rt rd sa 0b111011
dsrav rd rt rs      = op_3reg       0b000000 rs rt rd 0b010111
dsra32 rd rt sa     = op_2reg_sa    0b000000 rt rd sa 0b111111
dsrl rd rt sa       = op_2reg_sa    0b000000 rt rd sa 0b111010
dsrlv rd rt rs      = op_3reg       0b000000 rs rt rd 0b010110
dsrl32 rd rt sa     = op_2reg_sa    0b000000 rt rd sa 0b111110
dsub rd rs rt       = op_3reg       0b000000 rs rt rd 0b101110
dsubu rd rs rt      = op_3reg       0b000000 rs rt rd 0b101111
eret                = op_copz       0b0100 0          0b011000
j target            = op_jump       0b000010 target
jal target          = op_jump       0b000011 target
jalrr rd rs         = op_2reg_rsrd  0b000000 rs rd    0b001001
jalr rs             = jalrr 31 rs
jr rs               = op_1reg       0b000000 rs       0b001000
lb rt offs base     = op_2reg_imm   0b100000 base rt offs
lbu rt offs base    = op_2reg_imm   0b100100 base rt offs
ld rt offs base     = op_2reg_imm   0b110111 base rt offs
ldc1 rt offs base   = op_2reg_imm   0b110101 base rt offs
ldc2 rt offs base   = op_2reg_imm   0b110110 base rt offs
ldc3 rt offs base   = op_2reg_imm   0b110111 base rt offs
ldl rt offs base    = op_2reg_imm   0b011010 base rt offs
ldr rt offs base    = op_2reg_imm   0b011011 base rt offs
lh rt offs base     = op_2reg_imm   0b011011 base rt offs

hex n e = printf "0x%0*x\n" n e
