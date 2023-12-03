{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (isDigit)
import Control.Applicative (liftA2)
import Data.Array ((!), accumArray)

munchInts :: String -> [(Int, Int, Int)]
munchInts = go 0
  where
  go _ [] = []
  go i s@(c:cs)
    | isDigit c =
      let (n,cs) = span isDigit s
          ln = length n
      in (i, ln, read n) : go (i + ln) cs
    | otherwise =
      go (i + 1) cs

symb :: Char -> Bool
symb = not . liftA2 (||) isDigit (=='.')

main :: IO ()
main = interact
  $ show
  . liftA2
    (\nums symbs ->
        sum
      . map (\(_,_,_,n) -> n)
      . filter (\(x,y,l,_) ->
            (symbs ! (x-1,y))
         || (symbs ! (x-1,y-1))
         || (symbs ! (x-1,y+1))
         || (symbs ! (x+l,y))
         || (symbs ! (x+l,y-1))
         || (symbs ! (x+l,y+1))
         || any ((symbs !) . (,y-1) . (+x)) [0..l-1]
         || any ((symbs !) . (,y+1) . (+x)) [0..l-1] )
      $ nums)
    ( concat
    . zipWith (\y s -> map (\(x,l,n) -> (x,y,l,n)) $ munchInts s) [0..] )
    ( accumArray (const id) False ((-1 :: Int, -1 :: Int), (1000,1000))
    . concat
    . zipWith (\y s -> map (\(x,c) -> ((x,y), symb c)) s) [0..]
    . map (zip [0..]) )
  . filter (not . null) . lines
