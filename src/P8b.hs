module Main where
import Control.Applicative (liftA2)
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (scanl', foldl', findIndex)
import Data.Maybe (fromJust)

data Direction = DL | DR
  deriving (Eq, Show)

parseDirection :: Char -> Direction
parseDirection 'L' = DL
parseDirection 'R' = DR
parseDirection _ = error "invalid direction"

getDirection :: Direction -> (a, a) -> a
getDirection DL = fst
getDirection DR = snd

enc :: [Char] -> Int
enc = foldl' (\acc c -> acc * 26 + (ord c - ord 'A')) 0

main :: IO ()
main = interact
  $ show
  . foldl' lcm 1
  . liftA2
    (\m i ->
      fmap
      (\b ->
          fromJust
        $ findIndex ((=='Z') . last . fst . (m IM.!))
        $ scanl'
          (\c d -> getDirection d (snd $ m IM.! c))
          b i
      )
      ( map fst
      $ filter ((=='A') . last . fst . snd)
      $ IM.toList m)
    )
    ( IM.fromList
    . map
      ( (\k l r -> (enc k, (k, (l, r))))
    <$> (take 3 . drop 0)
    <*> (enc . take 3 . drop 7)
    <*> (enc . take 3 . drop 12)
      )
    . tail )
    ( cycle . map parseDirection . head )
  . filter (not . null) . lines
