module Main where
import Types
import Utility
import ImmutableBytes
import Story

import Text.Printf
import Data.Bits hiding (setBit, clearBit)

main :: IO ()
main = do story <- Story.load "minizork.z3"
          let version_address = ByteAddress 0
              version         = Story.readByte story version_address in
           printf "%d\n" version



