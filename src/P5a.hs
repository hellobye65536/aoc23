{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Applicative (liftA2, asum)
import Data.List.Split (splitOn, splitWhen)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

main :: IO ()
main = interact
  $ show
  . minimum
  . liftA2
    ( foldl'
      (\xs m ->
        map
          (\x ->
              fromMaybe x
            . asum
            $ map
              (\[ds,ss,l] ->
                (x-ss+ds) <$ guard (ss <= x && x < ss+l))
              m
          )
          xs
      )
    )
    ( map (read @Int) . splitOn " " . drop 7 . head )
    ( map (map
      ( map (read @Int)
      . words
      ))
    . splitWhen (not . isDigit . head)
    . tail . tail)
  . filter (not . null) . lines
