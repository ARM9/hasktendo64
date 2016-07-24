{-# LANGUAGE BinaryLiterals #-}
module Asm.Mips where

import           Data.Bits
import           Data.Word
import           Text.Printf

-- | Return an integer with the n least significant bits set
bits :: (Integral a, Integral b, Bits b) => a -> b
bits n = 1 `shiftL` fromIntegral n - 1

-- | Mask the n least significant bits of x
maskBits :: Word32 -> Word32 -> Word32
maskBits n x = bits n .&. x

opcode :: Word32 -> Word32 -> Word32
opcode op funct = (op `shiftL` 26) .|. funct

op_3reg :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_3reg op rs rt rd funct =
    opcode op funct .|. fields
    where
        fields = (rs `shiftL` 21) .|. (rt `shiftL` 16) .|. (rd `shiftL` 11)

op_2reg_rsrt :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_rsrt op rs rt funct =
    op_3reg op rs rt 0 funct

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
op_2reg_offs op rs rt offs =
    op_2reg_imm op rs rt (fromIntegral $ offs `shiftR` 2)

op_2reg_sa :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_sa op rt rd sa funct =
    opcode op funct .|. fields
    where
        fields = (rt `shiftL` 16) .|. (rd `shiftL` 11) .|. (maskBits 5 sa `shiftL` 6)

op_2reg_code :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_2reg_code op rs rt code funct =
    opcode op funct .|. fields .|. (maskBits 10 code `shiftL` 6)
    where
        fields = (rs `shiftL` 21) .|. (rt `shiftL` 16)

op_1reg_imm :: Word32 -> Word32 -> Word16 -> Word32
op_1reg_imm op rt imm =
    op_2reg_imm op 0 rt imm

op_1rs :: Word32 -> Word32 -> Word32 -> Word32
op_1rs op rs funct =
    op_3reg op rs 0 0 funct

op_1rd :: Word32 -> Word32 -> Word32 -> Word32
op_1rd op rd funct =
    op_3reg op 0 0 rd funct

copOpcode :: Word32 -> Word32 -> Word32 -> Word32
copOpcode op cp subop = (op `shiftL` 28) .|. (cp `shiftL` 26) .|. (subop `shiftL` 21)

-- | branch coprocessor Z condition
op_bcopzc :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_bcopzc op cp subop cond offs =
    copOpcode op cp subop .|. (cond `shiftL` 16) .|. maskBits 16 (offs `shiftR` 2)

op_copz_mov :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
op_copz_mov op cp subop rt rd =
    copOpcode op cp subop .|. (rt `shiftL` 16) .|. (rd `shiftL` 11)

op_copz :: Word32 -> Word32 -> Word32 -> Word32
op_copz op cp cofun =
    copOpcode op cp 0 .|. (1 `shiftL` 25) .|. maskBits 25 cofun

op_jump :: Word32 -> Word32 -> Word32
op_jump op target = (op `shiftL` 26) .|. maskBits 26 (target `shiftR` 2)

add rd rs rt        = op_3reg       0b000000 rs rt rd 0b100000
addi rt rs imm      = op_2reg_imm   0b001000 rs rt imm
addiu rt rs imm     = op_2reg_imm   0b001001 rs rt imm
addu rd rs rt       = op_3reg       0b000000 rs rt rd 0b100001
and rd rs rt        = op_3reg       0b000000 rs rt rd 0b100100
andi rt rs imm      = op_2reg_imm   0b001100 rs rt imm
bc0f offs           = op_bcopzc     0b0100 0 0b01000 0b00000 offs
bc0fl offs          = op_bcopzc     0b0100 0 0b01000 0b00010 offs
bc0t offs           = op_bcopzc     0b0100 0 0b01000 0b00001 offs
bc0tl offs          = op_bcopzc     0b0100 0 0b01000 0b00011 offs
bc1f offs           = op_bcopzc     0b0100 1 0b01000 0b00000 offs
bc1fl offs          = op_bcopzc     0b0100 1 0b01000 0b00010 offs
bc1t offs           = op_bcopzc     0b0100 1 0b01000 0b00001 offs
bc1tl offs          = op_bcopzc     0b0100 1 0b01000 0b00011 offs
bc2f offs           = op_bcopzc     0b0100 2 0b01000 0b00000 offs
bc2fl offs          = op_bcopzc     0b0100 2 0b01000 0b00010 offs
bc2t offs           = op_bcopzc     0b0100 2 0b01000 0b00001 offs
bc2tl offs          = op_bcopzc     0b0100 2 0b01000 0b00011 offs
bc3f offs           = op_bcopzc     0b0100 3 0b01000 0b00000 offs
bc3fl offs          = op_bcopzc     0b0100 3 0b01000 0b00010 offs
bc3t offs           = op_bcopzc     0b0100 3 0b01000 0b00001 offs
bc3tl offs          = op_bcopzc     0b0100 3 0b01000 0b00011 offs
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
break code          = (maskBits 20 code `shiftL` 6) .|. 0b001101
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
ddiv rs rt          = op_2reg_rsrt  0b000000 rs rt    0b011110
ddivu rs rt         = op_2reg_rsrt  0b000000 rs rt    0b011111
div rs rt           = op_2reg_rsrt  0b000000 rs rt    0b011010
divu rs rt          = op_2reg_rsrt  0b000000 rs rt    0b011011
dmfc0 rt rd         = op_copz_mov   0b0100 0 0b00001 rt rd
dmtc0 rt rd         = op_copz_mov   0b0100 0 0b00101 rt rd
dmult rs rt         = op_2reg_rsrt  0b000000 rs rt    0b011100
dmultu rs rt        = op_2reg_rsrt  0b000000 rs rt    0b011101
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
jalrr rd rs         = op_2reg_rsrd  0b000000 rs    rd 0b001001
jalr rs             = jalrr 31 rs
jr rs               = op_1rs        0b000000 rs       0b001000
lb rt offs base     = op_2reg_imm   0b100000 base rt offs
lbu rt offs base    = op_2reg_imm   0b100100 base rt offs
ld rt offs base     = op_2reg_imm   0b110111 base rt offs
ldc1 rt offs base   = op_2reg_imm   0b110101 base rt offs
ldc2 rt offs base   = op_2reg_imm   0b110110 base rt offs
ldc3 rt offs base   = op_2reg_imm   0b110111 base rt offs
ldl rt offs base    = op_2reg_imm   0b011010 base rt offs
ldr rt offs base    = op_2reg_imm   0b011011 base rt offs
lh rt offs base     = op_2reg_imm   0b100001 base rt offs
lhu rt offs base    = op_2reg_imm   0b100101 base rt offs
ll rt offs base     = op_2reg_imm   0b110000 base rt offs
lld rt offs base    = op_2reg_imm   0b110100 base rt offs
lui rt imm          = op_1reg_imm   0b001111 rt imm
lw rt offs base     = op_2reg_imm   0b100011 base rt offs
lwc1 rt offs base   = op_2reg_imm   0b110001 base rt offs
lwc2 rt offs base   = op_2reg_imm   0b110010 base rt offs
lwc3 rt offs base   = op_2reg_imm   0b110011 base rt offs
lwl rt offs base    = op_2reg_imm   0b100010 base rt offs
lwr rt offs base    = op_2reg_imm   0b100110 base rt offs
lwu rt offs base    = op_2reg_imm   0b100111 base rt offs
mfc0 rt rd          = op_copz_mov   0b0100 0 0b00000 rt rd
mfc1 rt rd          = op_copz_mov   0b0100 1 0b00000 rt rd
mfc2 rt rd          = op_copz_mov   0b0100 2 0b00000 rt rd
mfc3 rt rd          = op_copz_mov   0b0100 3 0b00000 rt rd
mfhi rd             = op_1rd        0b000000       rd 0b010000
mflo rd             = op_1rd        0b000000       rd 0b010010
mtc0 rt rd          = op_copz_mov   0b0100 0 0b00100 rt rd
mtc1 rt rd          = op_copz_mov   0b0100 1 0b00100 rt rd
mtc2 rt rd          = op_copz_mov   0b0100 2 0b00100 rt rd
mtc3 rt rd          = op_copz_mov   0b0100 3 0b00100 rt rd
mthi rs             = op_1rs        0b000000 rs       0b010001
mtlo rs             = op_1rs        0b000000 rs       0b010011
mult rs rt          = op_2reg_rsrt  0b000000 rs rt    0b011000
multu rs rt         = op_2reg_rsrt  0b000000 rs rt    0b011001
nor rd rs rt        = op_3reg       0b000000 rs rt rd 0b100111
or rd rs rt         = op_3reg       0b000000 rs rt rd 0b100101
ori rt rs imm       = op_2reg_imm   0b001101 rs rt imm
sb rt offs base     = op_2reg_imm   0b101000 base rt offs
sc rt offs base     = op_2reg_imm   0b111000 base rt offs
scd rt offs base    = op_2reg_imm   0b111100 base rt offs
sd rt offs base     = op_2reg_imm   0b111111 base rt offs
sdc1 rt offs base   = op_2reg_imm   0b111101 base rt offs
sdc2 rt offs base   = op_2reg_imm   0b111110 base rt offs
sdc3 rt offs base   = op_2reg_imm   0b111111 base rt offs
sdl rt offs base    = op_2reg_imm   0b101100 base rt offs
sdr rt offs base    = op_2reg_imm   0b101101 base rt offs
sh rt offs base     = op_2reg_imm   0b101001 base rt offs
sll rd rt sa        = op_2reg_sa    0b000000 rt rd sa 0b000000
sllv rd rt rs       = op_3reg       0b000000 rs rt rd 0b000100
slt rd rs rt        = op_3reg       0b000000 rs rt rd 0b101010
slti rt rs imm      = op_2reg_imm   0b001010 rs rt imm
sltiu rt rs imm     = op_2reg_imm   0b001011 rs rt imm
sltu rd rs rt       = op_3reg       0b000000 rs rt rd 0b101011
sra rd rt sa        = op_2reg_sa    0b000000 rt rd sa 0b000011
srav rd rt rs       = op_3reg       0b000000 rs rt rd 0b000111
srl rd rt sa        = op_2reg_sa    0b000000 rt rd sa 0b000010
srlv rd rt rs       = op_3reg       0b000000 rs rt rd 0b000110
sub rd rs rt        = op_3reg       0b000000 rs rt rd 0b100010
subu rd rs rt       = op_3reg       0b000000 rs rt rd 0b100011
sw rt offs base     = op_2reg_imm   0b101011 base rt offs
swc1 rt offs base   = op_2reg_imm   0b111001 base rt offs
swc2 rt offs base   = op_2reg_imm   0b111010 base rt offs
swc3 rt offs base   = op_2reg_imm   0b111011 base rt offs
swl rt offs base    = op_2reg_imm   0b101010 base rt offs
swr rt offs base    = op_2reg_imm   0b101110 base rt offs
sync                = 0b001111 :: Word32
syscall             = 0b001100 :: Word32
teq rs rt           = op_2reg_rsrt  0b000000 rs rt    0b110100
teqc rs rt c        = op_2reg_code  0b000000 rs rt c  0b110100
teqi rs imm         = op_2reg_imm   0b000001 rs 0b01100 imm
tge rs rt           = op_2reg_rsrt  0b000000 rs rt    0b110000
tgec rs rt c        = op_2reg_code  0b000000 rs rt c  0b110000
tgei rs imm         = op_2reg_imm   0b000001 rs 0b01000 imm
tgeiu rs imm        = op_2reg_imm   0b000001 rs 0b01001 imm
tgeu rs rt          = op_2reg_rsrt  0b000000 rs rt    0b110001
tgeuc rs rt c       = op_2reg_code  0b000000 rs rt c  0b110001
tlbp, tlbr, tlbwi, tlbwr :: Word32
tlbp                = (0b0100001 `shiftL` 25) .|. 0b001000
tlbr                = (0b0100001 `shiftL` 25) .|. 0b000001
tlbwi               = (0b0100001 `shiftL` 25) .|. 0b000010
tlbwr               = (0b0100001 `shiftL` 25) .|. 0b000110
tlt rs rt           = op_2reg_rsrt  0b000000 rs rt    0b110010
tltc rs rt c        = op_2reg_code  0b000000 rs rt c  0b110010
tlti rs imm         = op_2reg_imm   0b000001 rs 0b01010 imm
tltiu rs imm        = op_2reg_imm   0b000001 rs 0b01011 imm
tltu rs rt          = op_2reg_rsrt  0b000000 rs rt    0b110011
tltuc rs rt c       = op_2reg_code  0b000000 rs rt c  0b110011
tne rs rt           = op_2reg_rsrt  0b000000 rs rt    0b110110
tnec rs rt c        = op_2reg_code  0b000000 rs rt c  0b110110
tnei rs imm         = op_2reg_imm   0b000001 rs 0b01110 imm
xor rd rs rt        = op_3reg       0b000000 rs rt rd 0b100110
xori rt rs imm      = op_2reg_imm   0b001110 rs rt imm

hex e = printf "0x%08x\n" e
