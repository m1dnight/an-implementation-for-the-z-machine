module Main where
import Types
import Utility
import ImmutableBytes

import Text.Printf
import Data.Bits hiding (setBit, clearBit)

word :: Int
word = 0xBEEF

fetchBitsOriginal high length word =
  let mask = complement ((-1 :: Int) `shiftL` length)
  in
    shiftR word (high - length + 1) .&. mask




main :: IO ()
main = do let addr1   = ByteAddress 1
              bytes_a = make "Hello World"
              bytes_b = writeByte bytes_a addr1 65
              b_a     = readByte bytes_a addr1
              b_b     = readByte bytes_b addr1 in
            printf "%d %d\n" b_a b_b
