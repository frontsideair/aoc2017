{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (Right, Left, head, map)
import Data.Vector (Vector, fromList, elemIndex, head, (!))
import Data.Maybe (Maybe (Nothing, Just), fromJust)
import Control.Applicative

type Coord = (Int, Int)
type Map = Vector (Vector Char)
data Direction = Up | Right | Down | Left deriving (Enum, Show)
data Game = Game { _map :: Map, _coord :: Coord, _dir :: Direction, _letters :: [Char], _steps :: Int } deriving Show

next :: Direction -> Direction 
next Left = Up
next dir = succ dir

prev :: Direction -> Direction 
prev Up = Left
prev dir = pred dir

index :: Coord -> Map -> Char
index (y, x) map = map ! y ! x

inc :: Coord -> Direction -> Coord
inc (y, x) Up    = (y - 1, x)
inc (y, x) Down  = (y + 1, x)
inc (y, x) Left  = (y, x - 1)
inc (y, x) Right = (y, x + 1)

peek :: Map -> Direction -> Coord -> Maybe Direction
peek map direction coord = case index (inc coord direction) map of
  ' ' -> Nothing
  _   -> Just direction

pickDir :: Map -> Direction -> Coord -> Maybe Direction
pickDir map direction coord =
  peek map direction coord
    <|> peek map (next direction) coord
    <|> peek map (prev direction) coord

play :: Game -> Game
play game@(Game map coord direction letters steps) = case index coord map of
  ' ' -> game -- end
  '|' -> play $ game { _coord = inc coord direction, _steps = steps + 1 } -- keep going
  '-' -> play $ game { _coord = inc coord direction, _steps = steps + 1 } -- keep going
  '+' -> play $ game { _coord = inc coord newDir, _dir = newDir, _steps = steps + 1 } -- a crossroads, decide a direction
    where newDir = fromJust $ pickDir map direction coord
  l -> play $ game { _coord = inc coord direction, _letters = l : letters, _steps = steps + 1 } -- must be a letter, collect

main :: IO ()
main = do
  file <- readFile "./19-input.txt"
  let map :: Map     = fromList (fromList <$> lines file)
  let i              = fromJust $ elemIndex '|' (head map)
  let start :: Coord = (0, i)
  print start
  print $ play $ Game map start Down [] 0