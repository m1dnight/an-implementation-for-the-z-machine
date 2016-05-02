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
          let zstring = abbreviationZString story (Abbreviation 0)
              text    = displayBytes story zstring
          putStrLn text
          let zstring' = abbreviationZString story (Abbreviation 4)
              text'    = displayBytes story zstring'
          putStrLn text'



