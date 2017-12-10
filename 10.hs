import Text.Printf
import Data.Char
import Data.Bits
import Data.List.Split

input :: String
input = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"

data State = State {
  steps :: [Int]
, list :: [Int]
, skip :: Int
, pos :: Int
} deriving Show

initialState :: State
-- initialState = State [3, 4, 1, 5] [0 .. 4] 0 0
initialState = State
  { steps = read <$> splitOn "," input
  , list  = [0 .. 255]
  , skip  = 0
  , pos   = 0
  }

initialState2 :: State
-- initialState = State [3, 4, 1, 5] [0 .. 4] 0 0
initialState2 = State
  { steps = (concat . replicate 64) (map ord input ++ [17, 31, 73, 47, 23])
  , list  = [0 .. 255]
  , skip  = 0
  , pos   = 0
  }

rotateLeft :: Int -> [a] -> [a]
rotateLeft n l = take (length l) $ drop n (cycle l)

rotateRight :: Int -> [a] -> [a]
rotateRight n l = rotateLeft (length l - n) l

run1 :: State -> State
run1 state@State { steps = [] }                          = state
run1 State { steps = x:xs, list = l, skip = s, pos = p } = run1 State
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

run2 :: State -> String
run2 state =
  let firstRound  = run1 state
      chunks      = chunksOf 16 (list firstRound)
      secondRound = map (foldl1 xor) chunks
      thirdRound  = concatMap (printf "%02x") secondRound
  in  thirdRound

main :: IO ()
main = do
  let x:y:_ = list (run1 initialState)
  print (x * y)
  print (run2 initialState2)