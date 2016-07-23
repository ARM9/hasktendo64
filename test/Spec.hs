import           Control.Monad
import           Text.Printf

import           Asm.Mips
import qualified VR4300

testVR4300 :: IO ()
testVR4300 = do
    return ()
    let cpu = VR4300.new {VR4300.r2 = 0x100000000, VR4300.r3 = 1}
    let program1 = [addu 2 2 3, addu 2 2 3]
        program2 = [addiu 20 0 4, addiu 31 0 4, addu 29 20 31]
    mapM_ print $ VR4300.run cpu program1
    putStrLn "-----------------"
    let states = VR4300.run VR4300.new program2
    mapM_ print states
    let t = VR4300.r29 (last states)
    when (t /= 8)
        (error $ printf "r29 = %x" t)

main :: IO ()
main = do
    testVR4300
    putStrLn "\nAll tests passed"
    return ()
