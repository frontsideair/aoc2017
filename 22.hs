import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec

type Index = (Int, Int)
data Node = Clean | Infected deriving (Show, Eq)
data Direction = U | R | D | L deriving (Enum, Show)
data Game = Game { _index :: Index, _dir :: Direction, _infected :: Int, _map :: Map Index Node } deriving Show

rotateR :: Direction -> Direction
rotateR U = R
rotateR R = D
rotateR D = L
rotateR L = U

rotateL :: Direction -> Direction
rotateL U = L
rotateL L = D
rotateL D = R
rotateL R = U

move :: Direction -> (Int, Int) -> (Int, Int)
move U (row, col) = (row - 1, col)
move R (row, col) = (row, col + 1)
move D (row, col) = (row + 1, col)
move L (row, col) = (row, col - 1)

nodeParser :: GenParser Char st Node
nodeParser = Infected <$ char '#' <|> Clean <$ char '.'

mapParser :: GenParser Char st [[Node]]
mapParser = (many nodeParser) `sepBy` char '\n'

step :: Game -> Game
step (Game (row, col) dir infected m) =
  let node                     = Map.lookup (row, col) m
      (node', dir', infected') = case node of
        Just Infected -> (Clean, rotateR dir, infected)
        _             -> (Infected, rotateL dir, infected + 1)
      (row', col') = move dir' (row, col)
  in  Game (row', col') dir' infected' (Map.insert (row, col) node' m)

main :: IO ()
main = do
  contents <- readFile "./22-input.txt"
  case parse mapParser "(stdin)" contents of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right map -> do
      -- print map
      let map' =
            Map.fromList
              $ [ ((i, j), node)
                | (i, row ) <- zip [1 ..] map
                , (j, node) <- zip [1 ..] row
                ]
      -- print map'
      print $ _infected $ iterate step (Game (13, 13) U 0 map') !! 10000