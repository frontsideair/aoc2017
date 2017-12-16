import KnotHash
import Data.List.Split

input :: String
input = "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164"

salt :: [Int]
salt = [17, 31, 73, 47, 23]

main :: IO ()
main = do
  let firstInput = read <$> splitOn "," input
      (a:b:_)    = hash firstInput
  print (a * b)
  print (hashWithSalt input salt)