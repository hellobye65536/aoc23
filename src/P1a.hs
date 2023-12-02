module Main where

import Data.Char (isDigit)
import Data.List (singleton)

main :: IO ()
main = interact $ show . sum . map (((+) <$> ((*10) . read . singleton . head) <*> (read . singleton . last)) . filter isDigit) . filter (not . null) . lines
