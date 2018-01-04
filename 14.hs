{-# LANGUAGE ScopedTypeVariables #-}

import KnotHash
import Text.Printf
import Data.List
import Data.Graph

input :: String
input = "oundnydw"
-- input = "flqrgnkx"

inputs :: [String]
inputs = map (\(n :: Int) -> input ++ "-" ++ show n) [0 .. 127]

ones :: String -> Int
ones = length . filter (== '1')

countOnes :: [String] -> Int
countOnes = foldl' (\acc xs -> ones xs + acc) 0

mkVertex :: Int -> Int -> Int -> Int
mkVertex len x y = x * len + y

hashesToEdges :: [[Char]] -> [Edge]
hashesToEdges hashes =
  let horizontalEdges =
        [ [(a, b), (b, a)]
        | (row, row', i) <- zip3 hashes (tail hashes) [0 ..]
        , (x  , y   , j) <- zip3 row row' [0 ..]
        , x == '1' && y == '1'
        , let a = mkVertex (length row) i j
        , let b = mkVertex (length row) (i + 1) j
        ]
      verticalEdges =
        [ [(a, b), (b, a)]
        | (row, i)  <- zip hashes [0 ..]
        , (x, y, j) <- zip3 row (tail row) [0 ..]
        , x == '1' && y == '1'
        , let a = mkVertex (length row) i j
        , let b = mkVertex (length row) i (j + 1)
        ]
      self =
        [ (a, a)
        | (row, i) <- zip hashes [0 ..]
        , (x  , j) <- zip row [0 ..]
        , x == '1'
        , let a = mkVertex (length row) i j
        ]
  in  concat horizontalEdges ++ concat verticalEdges ++ self

edgesToVertices :: [(Int, Int)] -> [Int]
edgesToVertices = foldl' (\acc (a, b) -> a : b : acc) []

main :: IO ()
main = do
  let hashes :: [String] =
        fmap ((concatMap (printf "%08b")) . hashWithSalt) inputs
  -- print hashes
  print $ countOnes hashes
  let edges'    = hashesToEdges hashes
  -- print edges
  let graph    = buildG (0, 100000) edges'
  -- print graph
  let vertices' = nub $ edgesToVertices edges'
  -- print vertices
  print $ length $ dfs graph vertices'