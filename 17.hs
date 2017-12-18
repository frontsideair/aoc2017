{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Data.List
import Data.Maybe
import Data.Sequence (Seq, index, elemIndexL, insertAt)
import qualified Data.Sequence as S

data Buffer = Buffer { _index :: !Int, _buffer :: !(Seq Int) } deriving Show

steps :: Int
steps = 377
-- steps = 3

iters :: Int
iters = 2017

iters' :: Int
iters' = 50000000

step :: Buffer -> Int -> Buffer
step (Buffer index buffer) val = Buffer nextIndex
                                        (insertAt nextIndex val buffer)
  where nextIndex = (index + steps) `rem` (length buffer) + 1

main :: IO ()
main = do
  let end      = foldl' step (Buffer 0 (S.singleton 0)) [1 .. iters]
  print $ _buffer end `index` ((_index end) + 1)
  let end'      = foldl' step (Buffer 0 (S.singleton 0)) [1 .. iters']
  print $ (_buffer end') `index` 1

-- main = defaultMain [
--   bgroup "17" [ bench "500" $ nf (\n -> _index $ foldl' step (Buffer 0 (S.singleton 0)) [0 .. n]) 500
--               , bench "5000" $ nf (\n -> _index $ foldl' step (Buffer 0 (S.singleton 0)) [0 .. n]) 5000
--               , bench "50000" $ nf (\n -> _index $ foldl' step (Buffer 0 (S.singleton 0)) [0 .. n]) 50000
--               , bench "500000" $ nf (\n -> _index $ foldl' step (Buffer 0 (S.singleton 0)) [0 .. n]) 500000
--               ]
--   ]