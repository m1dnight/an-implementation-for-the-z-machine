module Main where
import Types
import Utility
import ImmutableBytes
import Story
import ZString
import Dictionary

import Text.Printf
import Data.Bits hiding (setBit, clearBit)

-- Introduction:
-- The project is divided into a few files.

-- Dictionary.hs: The dictionary is the Haskell file that contains the
-- code to read data from the dictionary in the Z-file. The dictionary
-- contains a mapping from pointers to words such that they can be
-- abbreviated in the virtual machine file.


main :: IO ()
main = do story <- Story.load "minizork.z3"
          let dict = Dictionary.display story
          putStrLn dict
