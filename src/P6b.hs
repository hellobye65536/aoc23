{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Applicative (Applicative(liftA2))
import Data.Char (isDigit)

main :: IO ()
main = interact
  $ show
  . liftA2
    (\t d -> 
      length
    . filter (\t' -> d < (t-t') * t' )
    $ [1..t-1]
    )
    (!! 0)
    (!! 1)
  . map ( read @Int . filter isDigit )
  . filter (not . null) . lines
