import Data.Map.Strict
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)

data Direction = L | R deriving Show
type StateName = Char
data Instruction = Instruction { _write :: Int, _move :: Direction, _continue :: StateName } deriving Show
data State = State { _zero :: Instruction, _one :: Instruction } deriving Show
data Tape = Tape { _current :: Int, _left :: [Int], _right :: [Int] } deriving Show
data Program = Program { _begin :: StateName, _remainingSteps :: Int, _states :: Map StateName State, _tape :: Tape } deriving Show

readDir :: String -> Direction
readDir "left"  = L
readDir "right" = R

emptyTape :: Tape
emptyTape = Tape 0 [] []

moveLeft :: Tape -> Tape
moveLeft (Tape curr []       right) = Tape 0 [] (curr : right)
moveLeft (Tape curr (l:left) right) = Tape l left (curr : right)

moveRight :: Tape -> Tape
moveRight (Tape curr left []       ) = Tape 0 (curr : left) []
moveRight (Tape curr left (r:right)) = Tape r (curr : left) right

moveDir :: Direction -> Tape -> Tape
moveDir L = moveLeft
moveDir R = moveRight

read' :: Tape -> Int
read' = _current

write' :: Int -> Tape -> Tape
write' val (Tape _ left right) = Tape val left right

getInstruction :: Int -> State -> Instruction
getInstruction 0 = _zero
getInstruction 1 = _one

-- Begin in state A.
-- Perform a diagnostic checksum after 12425180 steps.

-- In state A:
--   If the current value is 0:
--     - Write the value 1.
--     - Move one slot to the right.
--     - Continue with state B.
--   If the current value is 1:
--     - Write the value 0.
--     - Move one slot to the right.
--     - Continue with state F.

begin :: GenParser Char st StateName
begin = string "Begin in state " *> anyChar <* string ".\n"

steps :: GenParser Char st Int
steps =
  string "Perform a diagnostic checksum after " *> int <* string " steps.\n\n"

stateName :: GenParser Char st StateName
stateName = string "In state " *> anyChar <* string ":\n"

branch :: GenParser Char st Int
branch = string "  If the current value is " *> int <* string ":\n"

write :: GenParser Char st Int
write = string "    - Write the value " *> int <* string ".\n"

move :: GenParser Char st Direction
move =
  readDir
    <$> (  string "    - Move one slot to the "
        *> (string "left" <|> string "right")
        <* string ".\n"
        )

continue :: GenParser Char st StateName
continue = string "    - Continue with state " *> anyChar <* string ".\n"

instruction :: GenParser Char st Instruction
instruction = Instruction <$> write <*> move <*> continue

state :: GenParser Char st (StateName, State)
state =
  (,)
    <$> stateName
    <*> (State <$> (branch *> instruction) <*> (branch *> instruction))

program :: GenParser Char st Program
program =
  Program
    <$> begin
    <*> steps
    <*> (fromList <$> (state `sepBy` string "\n"))
    <*> pure emptyTape

step :: Program -> Program
step program'@(Program _ 0 _ _) = program'
step (Program begin' steps' states tape) =
  let state'             = states ! begin'
      currentValue       = read' tape
      currentInstruction = getInstruction currentValue state'
      valueToWrite       = _write currentInstruction
      directionToMove    = _move currentInstruction
      stateToContinue    = _continue currentInstruction
  in  step $ Program stateToContinue
                     (steps' - 1)
                     states
                     (moveDir directionToMove (write' valueToWrite tape))

diagnose :: Program -> Int
diagnose program' =
  let Tape curr left right = _tape program' in sum left + sum right + curr

main :: IO ()
main = do
  content <- readFile "./25-input.txt"
  case parse program "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print r
      let result = step r
      -- print result
      print $ diagnose result