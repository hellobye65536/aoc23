module Main where

extrapolate :: [Int] -> Int
extrapolate xs | all (==0) xs = 0
extrapolate xs = last xs + extrapolate (zipWith (-) (tail xs) xs)

main :: IO ()
main = interact
  $ show
  . sum
  . map
    ( extrapolate
    . map read
    . words
    )
  . filter (not . null) . lines
