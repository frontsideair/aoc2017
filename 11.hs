{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split
import Data.Char

data Direction = N | NE | SE | S | SW | NW deriving (Read, Show, Eq, Ord)
data Offset = Offset { n :: Int, ne :: Int, se :: Int } deriving Show

initialOffset = Offset
  { n  = 0
  , ne = 0
  , se = 0
  }

walk :: Offset -> Direction -> Offset
walk offset direction = case direction of
  N  -> offset { n = n offset + 1 }
  NE -> offset { ne = ne offset + 1 }
  SE -> offset { se = se offset + 1 }
  S  -> offset { n = n offset - 1 }
  SW -> offset { ne = ne offset - 1 }
  NW -> offset { se = se offset - 1 }

walkAll :: [Direction] -> Offset
walkAll = foldl walk initialOffset

distance Offset { n, ne, se } = abs ne + max (abs n) (abs se)

walkAll2 :: [Direction] -> (Offset, Int)
walkAll2 = foldl
  (\(offset, acc) d -> (walk offset d, max acc (distance offset)))
  (initialOffset, 0)

main :: IO ()
main = do
  contents <- readFile "./11-input.txt"
  let directions = read <$> (splitOn "," (toUpper <$> contents))
  let offset     = walkAll directions
  print $ distance offset
  print $ walkAll2 directions