{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.List.Split (splitOn)

main :: IO ()
main = interact
  $ show @Int
  . sum
  . map
    ( (\[w,c] ->
        (\case
         0 -> 0
         n -> 2^(n-1)
        )
      . length
      . filter (`elem` w)
      $ c
    )
    . map words
    . splitOn " | "
    . drop 2
    . dropWhile (/=':')
    )
  . filter (not . null) . lines
