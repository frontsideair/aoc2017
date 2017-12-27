import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec

type Index = (Int, Int)
data Node = Clean | Infected deriving (Show, Eq, Read)
data Direction = U | R | D | L deriving (Enum, Show)
data Game = Game { _index :: Index, _dir :: Direction, _infected :: Int, _map :: Map Index Node } deriving Show

data Node' = Clean' | Weakened | Infected' | Flagged deriving (Show, Eq, Read)
data Game' = Game' { _index' :: Index, _dir' :: Direction, _infected' :: Int, _map' :: Map Index Node' } deriving Show

readNode :: Char -> Node
readNode '#' = Infected
readNode '.' = Clean

readNode' :: Char -> Node'
readNode' '#' = Infected'
readNode' '.' = Clean'

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

reverse' :: Direction -> Direction
reverse' U = D
reverse' R = L
reverse' D = U
reverse' L = R

infect :: Node' -> Node'
infect Clean'    = Weakened
infect Weakened  = Infected'
infect Infected' = Flagged
infect Flagged   = Clean'

changeDir :: Node' -> Direction -> Direction
changeDir Clean'    = rotateL
changeDir Weakened  = id
changeDir Infected' = rotateR
changeDir Flagged   = reverse'

move :: Direction -> (Int, Int) -> (Int, Int)
move U (row, col) = (row - 1, col)
move R (row, col) = (row, col + 1)
move D (row, col) = (row + 1, col)
move L (row, col) = (row, col - 1)

nodeParser :: GenParser Char st Char
nodeParser = char '#' <|> char '.'

mapParser :: GenParser Char st [[Char]]
mapParser = (many nodeParser) `sepBy` char '\n'

step :: Game -> Game
step (Game (row, col) dir infected m) =
  let node                     = Map.lookup (row, col) m
      (node', dir', infected') = case node of
        Just Infected -> (Clean, rotateR dir, infected)
        _             -> (Infected, rotateL dir, infected + 1)
      (row', col') = move dir' (row, col)
  in  Game (row', col') dir' infected' (Map.insert (row, col) node' m)

step' :: Game' -> Game'
step' (Game' (row, col) dir infected m) =
  let node = fromMaybe Clean' $ Map.lookup (row, col) m
      node' = infect node
      dir' = changeDir node dir
      infected' = infected + case node' of
          Infected' -> 1
          _         -> 0
      (row', col') = move dir' (row, col)
  in  Game' (row', col') dir' infected' (Map.insert (row, col) node' m)

mkGame :: [[Char]] -> Game
mkGame m = Game
  (13, 13)
  U
  0
  ( Map.fromList
    [ ((i, j), readNode node)
    | (i, row ) <- zip [1 ..] m
    , (j, node) <- zip [1 ..] row
    ]
  )

mkGame' :: [[Char]] -> Game'
mkGame' m = Game'
  (13, 13)
  U
  0
  ( Map.fromList
    [ ((i, j), readNode' node)
    | (i, row ) <- zip [1 ..] m
    , (j, node) <- zip [1 ..] row
    ]
  )

main :: IO ()
main = do
  contents <- readFile "./22-input.txt"
  case parse mapParser "(stdin)" contents of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right m -> do
      -- print m
      -- print $ _infected $ iterate step (mkGame m) !! 10000
      print $ _infected' $ iterate step' (mkGame' m) !! 10000000