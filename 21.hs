{-# LANGUAGE TupleSections #-}

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (transpose)
import Data.Map (Map)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Text.Parsec (char, count, eof, many, newline)
import Text.Parsec.Char (string)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "21-input.txt" >>= either (error . show) return
  -- print `traverse` (filter (\(k, v) -> length k /= 4) $ Map.toList input)
  let stepper = step input
  print $ length $ filter (== True) $ iterate stepper initialState !! 5
  -- print $ f 3 initialState
  -- print $ stepper initialState
  -- print `traverse` Map.toList input
  return ()

type State = [Bool]

type Rules = (Map [Bool] [Bool])

initialState :: State
initialState = [False, True, False, False, False, True, True, True, True]

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

f :: Int -> State -> [State]
f n xs = cells
  where
    size = sqrt' $ length xs
    numCells = size `div` n
    cell xs' = concat $ take n $ take n <$> iterate (drop size) xs'
    row xs'' = take numCells $ cell <$> iterate (drop n) xs''
    cells = concat $ take numCells $ row <$> iterate (drop (n * size)) xs

g :: Int -> [State] -> State
g n xs = concat rows
  where
    numCells = sqrt' $ length xs
    size = sqrt' . length . head $ xs
    rowParts = fmap (take size . fmap (take size) <$> iterate (drop size)) xs
    row = take numCells $ concat . concat . transpose . take numCells <$> iterate (drop numCells) rowParts
    rows = row

step :: Rules -> State -> State
step rules state = g size $ index rules <$> f size state where size = if even (length state) then 2 else 3

index :: (Ord k, Show k) => Map k v -> k -> v
index m k = case Map.lookup k m of
  Just v -> v
  Nothing -> error $ "Key not found: " ++ show k

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "21-input.txt" >>= either (error . show) return
  -- print `traverse` (filter (\(k, v) -> length k /= 4) $ Map.toList input)
  let stepper = step input
  print $ length $ filter (== True) $ iterate stepper initialState !! 18
  return ()

parser :: Parser Rules
parser = Map.fromList . concat <$> (rule `sepBy` newline) <* eof

rule :: Parser [([Bool], [Bool])]
rule = do
  lhs <- many pixel `sepBy` char '/'
  string " => "
  rhs <- try threes <|> fours
  return $ (,rhs) <$> rotations lhs

threes :: Parser [Bool]
threes = do
  row1 <- count 3 pixel
  char '/'
  row2 <- count 3 pixel
  char '/'
  row3 <- count 3 pixel
  return $ concat [row1, row2, row3]

fours :: Parser [Bool]
fours = do
  row1 <- count 4 pixel
  char '/'
  row2 <- count 4 pixel
  char '/'
  row3 <- count 4 pixel
  char '/'
  row4 <- count 4 pixel
  return $ concat [row1, row2, row3, row4]

pixel :: Parser Bool
pixel = True <$ char '#' <|> False <$ char '.'

rotations :: [[a]] -> [[a]]
rotations xs = concat <$> [xs1, xs2, xs3, xs4, xs5, xs6, xs7, xs8]
  where
    xs1 = xs
    xs2 = transpose xs
    xs3 = reverse xs
    xs4 = transpose xs3
    xs5 = reverse <$> xs
    xs6 = transpose xs5
    xs7 = reverse xs5
    xs8 = transpose xs7

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
