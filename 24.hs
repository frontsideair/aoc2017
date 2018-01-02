import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)

type Component = (Int, Int)
data Components = Components { _used :: [Component], _remaining :: Set Component } deriving Show

component :: GenParser Char st Component
component = (,) <$> int <* char '/' <*> int

components :: GenParser Char st Components
components = do
  xs <- component `sepBy` char '\n'
  return $ Components [(0, 0)] (Set.fromList xs)

includes :: Int -> Component -> Bool
includes x (a, b) = x == a || x == b

adjust :: Int -> Component -> Component
adjust x comp@(a, b) = if x == b then comp else (b, a)

step :: Components -> [Components]
step orig@(Components used remaining) =
  -- [ Components (adjust lhs x : used) (Set.delete x remaining)
  -- | x <- Set.toList remaining
  -- , let lhs = (fst . head) used
  -- , includes lhs x
  -- ]
  let lhs = (fst . head) used
      xs  = filter (includes lhs) (Set.toList remaining)
      f x = Components (adjust lhs x : used) (Set.delete x remaining)
      ret = map f xs
  in  if null xs then [orig] else ret >>= step

strength :: [Component] -> Int
strength = foldl' (\acc (a, b) -> acc + a + b) 0

main :: IO ()
main = do
  content <- readFile "./24-input.txt"
  case parse components "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print r
      let bridges = step r
      print $ length bridges
      print $ maximum $ (strength . _used) <$> bridges
      print $ strength $ foldl'
        ( \longest (Components used _) ->
          case compare (length used) (length longest) of
            GT -> used
            LT -> longest
            EQ -> if strength used > strength longest then used else longest
        )
        []
        bridges