{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.Split (splitOn)

data Color = Red | Green | Blue deriving (Eq, Show)

parseColor :: String -> Color
parseColor "red" = Red
parseColor "green" = Green
parseColor "blue" = Blue
parseColor _ = error "bad color"

tooMuch :: (Int, Color) -> Bool
tooMuch (i, c) =
  i > case c of
    Red -> 12
    Green -> 13
    Blue -> 14

main :: IO ()
main = interact
  $ show @Int
  . sum
  . map fst
  . filter (not . any (any tooMuch) . snd)
  . zip [1..]
  . map
    ( map
      ( map ((\[i,c] -> (read i, parseColor c)) . words)
      . splitOn ", " )
    . splitOn "; "
    . drop 2 . dropWhile (/= ':'))
  . lines
