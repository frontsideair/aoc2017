module KnotHash (hash, hashWithSalt, State(State, steps, list, skip, pos)) where

import Data.Bits
import Data.List.Split
import Data.Char

data State = State {
  steps :: [Int]
, list :: [Int]
, skip :: Int
, pos :: Int
} deriving Show

salt :: [Int]
salt = [17, 31, 73, 47, 23]

rotateLeft :: Int -> [a] -> [a]
rotateLeft n l = take (length l) $ drop n (cycle l)

rotateRight :: Int -> [a] -> [a]
rotateRight n l = rotateLeft (length l - n) l

hash :: [Int] -> [Int]
hash input = list $ hash_ State
  { steps = input
  , list  = [0 .. 255]
  , skip  = 0
  , pos   = 0
  }

hash_ :: State -> State
hash_ state@State { steps = [] }                          = state
hash_ State { steps = x:xs, list = l, skip = s, pos = p } = hash_ State
  { steps = xs
  , list  = knot
  , skip  = s + 1
  , pos   = (p + x + s) `mod` length l
  }
 where
  -- rotate
  rotated  = rotateLeft p l
  -- reversed + tail
  reversed = reverse (take x rotated) ++ drop x rotated
  -- rotate back
  knot     = rotateRight p reversed

hashWithSalt :: String -> [Int]
hashWithSalt input =
  let state = State
        { steps = (concat . replicate 64) (map ord input ++ salt)
        , list  = [0 .. 255]
        , skip  = 0
        , pos   = 0
        }
      firstRound  = hash_ state
      chunks      = chunksOf 16 (list firstRound)
      secondRound = map (foldl1 xor) chunks
  in  secondRound