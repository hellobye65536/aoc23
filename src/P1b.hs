module Main where

import Data.List (isPrefixOf)

nums :: [(Int, String)]
nums =
  [ (1, "one")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine")
  , (1, "1")
  , (2, "2")
  , (3, "3")
  , (4, "4")
  , (5, "5")
  , (6, "6")
  , (7, "7")
  , (8, "8")
  , (9, "9")
  ]

munch :: String -> [Int]
munch [] = []
munch s@(_ : t)
  | (i:_) <- map fst $ filter ((`isPrefixOf` s) . snd) nums = i : munch t
  | otherwise = munch t

main :: IO ()
main = interact $ show . sum . map (((+) <$> ((* 10) . head) <*> last) . filter (/=0) . munch) . filter (not . null) . lines
