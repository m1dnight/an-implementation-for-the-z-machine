module Main where
import Types
import Utility
import ImmutableBytes
import Story
import ZString
import Dictionary

import Text.Printf
import Data.Bits hiding (setBit, clearBit)

main :: IO ()
main = do story <- Story.load "minizork.z3"
          let dict = Dictionary.display story
          putStrLn dict
        
           



