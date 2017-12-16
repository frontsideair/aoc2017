import Text.Printf

initA = 873
initB = 583
-- initA = 65
-- initB = 8921

multA = 16807
multB = 48271

divider = 2147483647
iterations = 40000000
iterations' = 5000000

toBinary :: Int -> String
toBinary = printf "%032b"

same :: [Char] -> [Char] -> Bool
same a b = drop 16 a == drop 16 b

generate :: Int -> Int -> [Int]
generate init mult = drop 1 $ iterate (\n -> (n * mult) `rem` divider) init

filterA :: Int -> Bool
filterA n = n `rem` 4 == 0

filterB :: Int -> Bool
filterB n = n `rem` 8 == 0

generate' :: (Int -> Bool) -> Int -> Int -> [Int]
generate' f init mult =
  drop 1 $ filter f $ iterate (\n -> (n * mult) `rem` divider) init

listA = toBinary <$> generate initA multA
listB = toBinary <$> generate initB multB

listA' = toBinary <$> generate' filterA initA multA
listB' = toBinary <$> generate' filterB initB multB

zipped = zip listA listB
zipped' = zip listA' listB'

main :: IO ()
main = do
  print $ length $ filter (uncurry same) $ take iterations zipped
  print $ length $ filter (uncurry same) $ take iterations' zipped'