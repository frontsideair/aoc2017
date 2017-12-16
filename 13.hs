import Data.List
import Data.Monoid
import Text.ParserCombinators.Parsec

data Game = Game { _penalty :: Penalty, _layers :: [Layer], _offset :: Int } deriving Show
data Layer = Layer { _index :: Int, _range :: Int } deriving Show
data Penalty = NotCaught | Caught Int deriving (Show, Eq)

instance Monoid Penalty where
  mempty = NotCaught
  mappend NotCaught NotCaught = NotCaught
  mappend NotCaught (Caught a) = Caught a
  mappend (Caught a) NotCaught = Caught a
  mappend (Caught a) (Caught b) = Caught (a+b)

-- | Calculates scanner position of a layer
-- Examples:
-- >>> scannerPosition 0 5
-- 0
-- >>> scannerPosition 4 5
-- 4
-- >>> scannerPosition 5 5
-- 3
-- >>> scannerPosition 8 5
-- 0
-- >>> scannerPosition 16 5
-- 0
scannerPosition :: Int -> Int -> Int
scannerPosition time range = if remainder <= (range - 1)
  then remainder
  else halfway - remainder
 where
  halfway   = (range - 1) * 2
  remainder = time `rem` halfway

stepPenalty :: Int -> Penalty -> Layer -> Penalty
stepPenalty offset acc (Layer index range) =
  ( if scannerPosition (offset + index) range == 0
      then Caught (index * range)
      else NotCaught
    )
    <> acc

computeGame :: Game -> Game
computeGame game@(Game _ [] _) = game
computeGame (Game penalty (layer:layers) offset) =
  computeGame $ Game (stepPenalty offset penalty layer) layers offset

line :: GenParser Char st Layer
line = do
  i <- read <$> many digit
  _ <- string ": "
  r <- read <$> many digit
  return $ Layer i r

input :: GenParser Char st Game
input = do
  ls <- endBy line newline
  return $ Game NotCaught ls 0

delay :: Int -> Game -> Game
delay offset game = game { _offset = _offset game + offset }

main :: IO ()
main = do
  content <- readFile "./13-input.txt"
  case parse input "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right game -> do
      print game
      print $ computeGame game
      let games = iterate (delay 1) game
      print $ find (\g -> _penalty (computeGame g) == NotCaught) games
  return ()