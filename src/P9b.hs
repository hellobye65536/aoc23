module Main where

extrapolate :: [Int] -> Int
extrapolate xs | all (==0) xs = 0
extrapolate xs = head xs + extrapolate (zipWith (-) xs (tail xs))

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
