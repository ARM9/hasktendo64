{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString    as B
import           System.Environment
import           System.Exit

import qualified N64

usage :: IO a
usage = do
    n <- getProgName
    putStrLn $ n ++ " rom"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    rom_path <- case args of
        []      -> usage
        (r:_)   -> return r
    rom <- B.readFile rom_path
    let n64 = N64.newN64 rom
    mapM_ (print . N64.cpu) $ take 5 $ iterate N64.step n64
    --loop n64

loop :: N64.N64 -> IO ()
loop n64 = do
    print $ N64.cpu n64
    loop $ N64.step n64
