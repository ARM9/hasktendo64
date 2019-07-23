module Assembler.Test where

import           Asm.Mips
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable           (foldMap)
import           Prelude                 (IO, error, print, putStrLn, return,
                                          ($))
import           System.Process

testAssembler :: IO ()
testAssembler = do
    B.writeFile "testAssembler.bin" $ toLazyByteString $ foldMap word32BE everyOp
    _ <- createProcess $ shell "mips64-elf-objdump -D -b binary -mmips:4300 -EB -M gpr-names=numeric testAssembler.bin > test.dis && echo '// vim:ft=mips' >> test.dis"
    return ()
    where everyOp = [
              add 14 17 30
            , addi 30 17 0xffff
            , addiu 30 17 0xffff
            , addu 14 17 30
            , and 14 17 30
            , andi 30 17 0xffff
            , bc0f  0x7fffe
            , bc0fl 0x7fffe
            , bc0t  0x7fffe
            , bc0tl 0x7fffe
            , bc1f  0x7fffe
            , bc1fl 0x7fffe
            , bc1t  0x7fffe
            , bc1tl 0x7fffe
            , bc2f  0x7fffe
            , bc2fl 0x7fffe
            , bc2t  0x7fffe
            , bc2tl 0x7fffe
            , bc3f  0x7fffe
            , bc3fl 0x7fffe
            , bc3t  0x7fffe
            , bc3tl 0x7fffe
            , beq 17 30 0xffff
            , beql 17 30 0xffff
            , bgez 17 0xffff
            , bgezal 17 0xffff
            , bgezall 17 0xffff
            , bgezl 17 0xffff
            , bgtz 17 0xffff
            , bgtzl 17 0xffff
            , blez 17 0xffff
            , blezl 17 0xffff
            , bltz 17 0xffff
            , bltzal 17 0xffff
            , bltzall 17 0xffff
            , bltzl 17 0xffff
            , bne 17 30 0xffff
            , bnel 17 30 0xffff
            , break 0xffff
            , cache 1 27 0xffff
            , cfc1 30 14
            , cfc2 30 14
            , cfc3 30 14
            , cop0 52
            , cop1 (-1)
            , cop2 42
            , cop3 1337
            , ctc1 30 14
            , ctc2 30 14
            , ctc3 30 14
            , dadd 14 17 30
            , daddi 30 17 0xffff
            , daddiu 30 17 0xffff
            , daddu 14 17 30
            , ddiv 17 30
            , ddivu 17 30
            , div 17 30
            , divu 17 30
            , dmfc0 30 14
            , dmtc0 30 14
            , dmult 17 30
            , dmultu 17 30
            , dsll 14 30 30
            , dsllv 14 30 17
            , dsll32 14 30 30
            , dsra 14 30 30
            , dsrav 14 30 17
            , dsra32 14 30 30
            , dsrl 14 30 30
            , dsrlv 14 30 17
            , dsrl32 14 30 30
            , dsub 14 17 30
            , dsubu 14 17 30
            , eret
            , j (-1)
            , jal 0x08000008
            , jalrr 14 17
            , jalr 17
            , jr 17
            , lb 30 0xffff 27
            , lbu 30 0xffff 27
            , ld 30 0xffff 27
            , ldc1 30 0xffff 27
            , ldc2 30 0xffff 27
            , ldc3 30 0xffff 27 -- == ld on 64-bit
            , ldl 19 0xffff 29
            , ldr 19 0xffff 29
            , lh 19 0xffff 29
            , lhu 19 0xffff 29
            , ll 19 0xffff 29
            , lld 19 0xffff 29
            , lui 19 0xffff
            , lw 19 0xffff 29
            , lwc1 19 0xffff 29
            , lwc2 19 0xffff 29
            , lwc3 19 0xffff 29
            , lwl 19 0xffff 29
            , lwr 19 0xffff 29
            , lwu 19 0xffff 29
            , mfc0 19 30
            , mfc1 19 30
            , mfc2 19 30
            , mfc3 19 30
            , mfhi 30
            , mflo 30
            , mtc0 19 30
            , mtc1 19 30
            , mtc2 19 30
            , mtc3 19 30
            , mthi 27
            , mtlo 27
            , mult 27 19
            , multu 27 19
            , nor 30 27 19
            , or 30 27 19
            , ori 19 27 0xffff
            , sb 19 0xffff 29
            , sc 19 0xffff 29
            , scd 19 0xffff 29
            , sd 19 0xffff 29
            , sdc1 19 0xffff 29
            , sdc2 19 0xffff 29
            , sdc3 19 0xffff 29 -- sd on 64-bit
            , sdl 19 0xffff 29
            , sdr 19 0xffff 29
            , sh 19 0xffff 29
            , sll 30 19 30
            , sllv 30 19 27
            , slt 30 27 19
            , slti 19 27 0xffff
            , sltiu 19 27 0xffff
            , sltu 30 27 19
            , sra 30 19 30
            , srav 30 19 27
            , srl 30 19 30
            , srlv 30 19 27
            , sub 30 27 19
            , subu 30 27 19
            , sw 19 0xffff 29
            , swc1 19 0xffff 29
            , swc2 19 0xffff 29
            , swc3 19 0xffff 29
            , swl 19 0xffff 29
            , swr 19 0xffff 29
            , sync
            , syscall
            , teq 27 19
            , teqc 27 19 42
            , teqi 27 0xffff
            , tge 27 19
            , tgec 27 19 42
            , tgei 27 0xffff
            , tgeiu 27 0xffff
            , tgeu 27 19
            , tgeuc 27 19 42
            , tlbp
            , tlbr
            , tlbwi
            , tlbwr
            , tlt 27 19
            , tltc 27 19 42
            , tlti 27 0xffff
            , tltiu 27 0xffff
            , tltu 27 19
            , tltuc 27 19 42
            , tne 27 19
            , tnec 27 19 42
            , tnei 27 0xffff
            , xor 30 27 19
            , xori 19 27 0xffff
            ]
