{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.Split (splitOn)
import Data.Foldable (foldl')

data Color = Red | Green | Blue deriving (Eq, Show)

parseColor :: String -> Color
parseColor "red" = Red
parseColor "green" = Green
parseColor "blue" = Blue
parseColor _ = error "bad color"

maxColor :: (Int, Int, Int) -> (Int, Color) -> (Int, Int, Int)
maxColor (r, g, b) (i, c) = case c of
  Red   -> (max r i, g, b)
  Green -> (r, max g i, b)
  Blue  -> (r, g, max b i)

main :: IO ()
main = interact
  $ show @Int
  . sum
  . map
    ( (\(r,g,b) -> r*g*b)
    . foldl' maxColor (0,0,0)
    . concatMap
      ( map ((\[i,c] -> (read @Int i, parseColor c)) . words)
      . splitOn ", " )
    . splitOn "; "
    . drop 2 . dropWhile (/= ':'))
  . lines
