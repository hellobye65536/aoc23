{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char (isDigit)
import Control.Applicative (liftA2)
import Data.Array ((!), accumArray)
import Data.Maybe (mapMaybe)
import Data.List (sortOn, groupBy)
import Data.Function (on)

munchInts :: String -> [(Int, (Int, Int))]
munchInts = go 0 0
 where
  go _ _ [] = []
  go i x s = case span isDigit s of
    ([], _ : s) -> go i (x + 1) s
    (sn, s) ->
      let ln = length sn
          n = read sn
       in map ((,(i, n)) . (+ x)) [0 .. ln-1] ++ go (i + 1) (x + ln) s

main :: IO ()
main = interact
  $ show
  . sum
  . liftA2
    (\nums ->
      map (\(x,y) ->
        (\case
          [a,b] -> a*b
          _ -> 0
        )
        . map (snd . head)
        . groupBy ((==) `on` fst)
        . sortOn fst
        . mapMaybe (nums !)
        $ liftA2 (,) [x-1,x,x+1] [y-1,y,y+1]
      )
    )
    ( accumArray (const Just) Nothing ((-1 :: Int, -1 :: Int), (1000, 1000))
    . concatMap (uncurry (\y -> map (\(x,(i,n)) -> ((x,y),(i+1000*y,n)))))
    . zip [0..]
    . map munchInts
    )
    ( concatMap
      ( map fst
      . filter snd
      . uncurry (\y -> map (\(x,n) -> ((x,y),n)))
      )
    . zip [0..]
    . map (zip [0..] . map (=='*'))
    )
  . filter (not . null) . lines
