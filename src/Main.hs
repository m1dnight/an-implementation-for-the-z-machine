module Main where
import Types
import Utility
import Text.Printf
import Data.Bits hiding (setBit, clearBit)

word :: Int
word = 0xBEEF

fetchBitsOriginal high length word =
  let mask = complement ((-1 :: Int) `shiftL` length)
  in
    shiftR word (high - length + 1) .&. mask




main :: IO ()
main = do putStrLn $ printf "\n\n%x\n\n" (shiftR word 12 .&. complement (shiftL (-1 :: Int) 15))
          putStrLn $ printf "\n\n%x\n\n" (fetchBitsOriginal 15 4 word)
          putStrLn $ printf "\n\n%x\n\n" (fetchBits bit15 size4 word)
