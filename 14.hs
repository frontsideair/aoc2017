{-# LANGUAGE ScopedTypeVariables #-}

import KnotHash
import Text.Printf

input :: String
input = "oundnydw"
-- input = "flqrgnkx"

inputs :: [String]
inputs = map (\(n :: Int) -> input ++ "-" ++ show n) [0 .. 127]

ones :: String -> Int
ones = length . filter (=='1')

countOnes :: [String] -> Int
countOnes = foldl (\acc xs -> ones xs + acc) 0

main :: IO ()
main = do
  let hashes :: [String] =
        fmap ((concatMap (printf "%08b")) . hashWithSalt) inputs
  print $ countOnes hashes