{-# LANGUAGE TypeApplications #-}
module Main where
import Data.List.Split (splitOn)
import Data.List (foldl')

main :: IO ()
main = interact
  $ show @Int
  . fst
  . foldl'
    (\(acc,m:ms) [w,c] ->
      ( acc + m
      , let (p,s) = splitAt (length (filter (`elem` w) c)) ms
        in  map (+m) p ++ s)
    ) (0, repeat 1)
  . map
    ( map words
    . splitOn " | "
    . drop 2
    . dropWhile (/=':')
    )
  . filter (not . null) . lines
