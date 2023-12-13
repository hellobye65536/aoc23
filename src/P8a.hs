module Main where
import Control.Applicative (liftA2)
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, foldl', scanl')
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

begin, end :: Int
begin = enc "AAA"
end   = enc "ZZZ"

main :: IO ()
main = interact
  $ show
  . liftA2
    (\m ->
        fromJust
      . elemIndex end
      . scanl'
        (\c d -> getDirection d $ m IM.! c)
        begin
    )
    ( IM.fromList
    . map
      ( (\k l r -> (k, (l, r)))
    <$> (enc . take 3 . drop 0)
    <*> (enc . take 3 . drop 7)
    <*> (enc . take 3 . drop 12)
      )
    . tail )
    ( cycle . map parseDirection . head )
  . filter (not . null) . lines
