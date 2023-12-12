{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Applicative (Applicative(liftA2))

main :: IO ()
main = interact
  $ show
  . product
  . liftA2
    ( zipWith
      (\t d -> 
        length
      . filter (\t' -> d < (t-t') * t' )
      $ [1..t-1]
      )
    )
    head
    (head . tail)
  . map ( map (read @Int) . words . drop 1 . dropWhile (/=':') )
  . filter (not . null) . lines
