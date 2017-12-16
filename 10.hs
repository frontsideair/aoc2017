{-# LANGUAGE ScopedTypeVariables #-}

import KnotHash
import Data.List.Split
import Text.Printf

input :: String
input = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"

main :: IO ()
main = do
  let firstInput = read <$> splitOn "," input
      (a:b:_)    = hash firstInput
  print (a * b)
  let h :: String = concatMap (printf "%02x") (hashWithSalt input)
  print h