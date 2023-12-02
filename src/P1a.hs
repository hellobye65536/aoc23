module Main where

import Data.Char (isDigit, digitToInt)

main :: IO ()
main = interact
  $ show
  . sum
  . map
    ( ((+) <$> ((*10) . head) <*> last)
    . map digitToInt
    . filter isDigit)
  . filter (not . null) . lines
