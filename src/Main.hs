module Main where
import Types
import Utility
import ImmutableBytes
import Story
import ZString

import Text.Printf
import Data.Bits hiding (setBit, clearBit)

main :: IO ()
main = do story <- Story.load "minizork.z3"
          let zstring = ZString 0xb106
              text    = ZString.read story zstring
              output  = printf "%s\n" text
          putStrLn output
           



