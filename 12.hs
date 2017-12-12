import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec

type Graph =  Map Node [Node]
type Node = Int

empty :: Graph
empty = Map.empty

insert :: Graph -> (Node, [Node]) -> Graph
insert graph (n, neighbors) = Map.insert n neighbors graph

node :: GenParser Char st Node
node = read <$> many digit

line :: GenParser Char st (Node, [Node])
line = do
  n  <- node
  _  <- string " <-> "
  ns <- node `sepBy` string ", "
  return (n, ns)

input :: GenParser Char st [(Node, [Node])]
input = endBy line newline

dfs :: Graph -> Set Node -> Node -> Set Node
dfs graph acc start = if Set.member start acc
  then acc
  else foldl (dfs graph) (Set.insert start acc) (graph ! start)

main :: IO ()
main = do
  content <- readFile "./12-input.txt"
  case parse input "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      let graph = foldl insert empty r
      -- print graph
      print $ length $ dfs graph Set.empty 0
      let groups = dfs graph Set.empty <$> Map.keys graph
      print $ length $ Set.fromList groups
  return ()