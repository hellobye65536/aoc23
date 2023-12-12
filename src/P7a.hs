{-# LANGUAGE TypeApplications #-}
module Main where
import Data.List (sort, group, sortOn)
import Control.Applicative (Applicative(liftA2))
import Data.Maybe (fromJust)

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA
  deriving (Eq, Ord, Show)

data Hand = HH | HP | H2P | H3 | HF | H4 | H5
  deriving (Eq, Ord, Show)

cards :: [(Char, Card)]
cards =
  [ ('2', C2)
  , ('3', C3)
  , ('4', C4)
  , ('5', C5)
  , ('6', C6)
  , ('7', C7)
  , ('8', C8)
  , ('9', C9)
  , ('T', CT)
  , ('J', CJ)
  , ('Q', CQ)
  , ('K', CK)
  , ('A', CA)
  ]

getHand :: [Card] -> Hand
getHand l@[_, _, _, _, _] = case gl of
  [(_, 5)]                         -> H5
  [(_, 1), (_, 4)]                 -> H4
  [(_, 2), (_, 3)]                 -> HF
  [(_, 1), (_, 1), (_, 3)]         -> H3
  [(_, 1), (_, 2), (_, 2)]         -> H2P
  [(_, 1), (_, 1), (_, 1), (_, 2)] -> HP
  _                                -> HH
  where gl = sortOn snd $ map (liftA2 (,) head length) $ group $ sort l
getHand _ = error "not 5 cards"

main :: IO ()
main = interact
  $ show
  . sum
  . zipWith (*) [1..]
  . map snd
  . sortOn ( liftA2 (,) getHand id . fst )
  . map
    ( liftA2
      (,)
      (map (fromJust . flip lookup cards) . (!! 0))
      (read @Int . (!! 1))
    . words
    )
  . filter (not . null) . lines
