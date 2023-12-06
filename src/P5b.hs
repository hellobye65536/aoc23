{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Foldable (Foldable (foldMap'))
import Data.List (foldl', unfoldr)
import Data.List.Split (chunksOf, splitOn, splitWhen)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Semigroup (Min (Min, getMin))

main :: IO ()
main = interact
  $ show
  . minimum
  . map fst
  . liftA2
    ( foldl'
      (\xs m ->
        xs >>= unfoldr (\(x,l) ->
          let (yx,yl) = fromMaybe
                ( x
                , maybe
                  l
                  ( min l . getMin )
                  ( foldMap' (Just . Min)
                  . filter (> 0)
                  $ map (\(_ds,ss,_ml) -> ss-x) m
                  )
                )
                ( listToMaybe
                $ mapMaybe (\(ds,ss,ml) ->
                  if ss <= x && x < ss + ml
                    then Just (x-ss+ds, min l (ss+ml-x))
                    else Nothing
                  ) m
                )
          in ((yx,yl), (x+yl, l-yl)) <$ guard (l > 0)
      ))
    )
    ( map (\[a,b] -> (a,b))
    . chunksOf 2
    . map (read @Int)
    . splitOn " "
    . drop 7
    . head
    )
    ( map (map
      ( (\[a,b,c] -> (a,b,c))
      . map (read @Int)
      . words
      ))
    . splitWhen (not . isDigit . head)
    . tail . tail
    )
  . filter (not . null) . lines
