import Text.ParserCombinators.Parsec
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V

type Position = Int
type Program = Char
data Move = Spin Int | Exchange Position Position | Partner Program Program deriving Show

programs :: V.Vector Char
programs = V.fromList ['a' .. 'p']

dance :: GenParser Char st (V.Vector Move)
dance = V.fromList <$> (spin <|> exchange <|> partner) `sepBy` char ','

spin :: GenParser Char st Move
spin = do
  _ <- char 's'
  s <- many $ noneOf ","
  return $ Spin (read s)

exchange :: GenParser Char st Move
exchange = do
  _ <- char 'x'
  a <- many $ noneOf "/,"
  _ <- char '/'
  b <- many $ noneOf ","
  return $ Exchange (read a) (read b)

partner :: GenParser Char st Move
partner = do
  _ <- char 'p'
  a <- noneOf "/,"
  _ <- char '/'
  b <- noneOf ","
  return $ Partner a b

-- | Swaps two elements
-- Examples:
-- >>> swap [1,2] 0 1
-- [2,1]
-- >>> swap [1,2,3] 0 1
-- [2,1,3]
-- >>> swap [1,2,3] 0 2
-- [3,2,1]
-- >>> swap [1,2,3] 2 0
-- [3,2,1]
swap :: V.Vector Char -> Int -> Int -> V.Vector Char
swap l a b = (V.//) l [(a, b'), (b, a')]
 where
  a' = (V.!) l a
  b' = (V.!) l b
  -- let m         = min a b
  --     n         = max a b
  --     (x, y:ys) = splitAt m l
  --     (z, t:ts) = splitAt (n - m - 1) ys
  -- in  x ++ t : z ++ y : ts

findPos :: Char -> V.Vector Char -> Int
findPos x xs = fromJust $ V.elemIndex x xs

rotateRight :: Int -> V.Vector a -> V.Vector a
rotateRight n v = right <> left
  where (left, right) = V.splitAt (V.length v - n) v

consume :: V.Vector Char -> Move -> V.Vector Char
consume ps (Spin n      ) = rotateRight n ps
consume ps (Exchange a b) = swap ps a b
consume ps (Partner  a b) = swap ps (findPos a ps) (findPos b ps)

mult :: Int
mult = 1000000000

run :: V.Vector Move -> V.Vector Char -> V.Vector Char
run r p = foldl' consume p r

main :: IO ()
main = do
  content <- readFile "./16-input.txt"
  case parse dance "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print $ run r programs
      let period =
            let (x:xs) = iterate (run r) programs
            in  1 + length (takeWhile (/= x) xs)
          iterations = mult `rem` period
      print $ iterate (run r) programs !! iterations