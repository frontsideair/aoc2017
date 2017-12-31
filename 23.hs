import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Vector

data Instruction = Set Reg Val
                 | Sub Reg Val
                 | Mul Reg Val
                 | Jnz Val Val deriving Show
type Reg = Char
data Val = RegVal Reg | IntVal Int deriving Show
type Program = Vector Instruction
data Registers = Registers { _a :: Int, _b :: Int, _c :: Int, _d :: Int, _e :: Int, _f :: Int, _g :: Int, _h :: Int } deriving Show
data State = State { _program :: Program
                   , _pc :: Int
                   , _num_mul :: Int
                   , _registers :: Registers } deriving Show

val :: GenParser Char st Val
val = IntVal <$> int <|> RegVal <$> anyChar

set :: GenParser Char st Instruction
set = Set <$ try (string "set ") <*> anyChar <* char ' ' <*> val

sub :: GenParser Char st Instruction
sub = Sub <$ try (string "sub ") <*> anyChar <* char ' ' <*> val

mul :: GenParser Char st Instruction
mul = Mul <$ try (string "mul ") <*> anyChar <* char ' ' <*> val

jnz :: GenParser Char st Instruction
jnz = Jnz <$ try (string "jnz ") <*> val <* char ' ' <*> val

program :: GenParser Char st Program
program = fromList <$> (set <|> sub <|> mul <|> jnz) `sepBy` char '\n'

getVal :: Val -> Registers -> Int
getVal (IntVal i) _         = i
getVal (RegVal r) registers = getReg r registers

getReg :: Reg -> Registers -> Int
getReg 'a' = _a
getReg 'b' = _b
getReg 'c' = _c
getReg 'd' = _d
getReg 'e' = _e
getReg 'f' = _f
getReg 'g' = _g
getReg 'h' = _h

setReg :: Int -> Reg -> Registers -> Registers
setReg val 'a' registers = registers { _a = val }
setReg val 'b' registers = registers { _b = val }
setReg val 'c' registers = registers { _c = val }
setReg val 'd' registers = registers { _d = val }
setReg val 'e' registers = registers { _e = val }
setReg val 'f' registers = registers { _f = val }
setReg val 'g' registers = registers { _g = val }
setReg val 'h' registers = registers { _h = val }

run :: State -> State
run state@(State prog pc acc registers) = case prog !? pc of
  Just (Set lhs rhs) ->
    run $ State prog (pc + 1) acc (setReg (getVal rhs registers) lhs registers)
  Just (Sub lhs rhs) -> run $ State
    prog
    (pc + 1)
    acc
    (setReg ((getReg lhs registers) - (getVal rhs registers)) lhs registers)
  Just (Mul lhs rhs) -> run $ State
    prog
    (pc + 1)
    (acc + 1)
    (setReg ((getReg lhs registers) * (getVal rhs registers)) lhs registers)
  Just (Jnz lhs rhs) -> run $ State
    prog
    (pc + if getVal lhs registers == 0 then 1 else getVal rhs registers)
    acc
    registers
  Nothing -> state

main :: IO ()
main = do
  content <- readFile "./23-input.txt"
  case parse program "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print r
      print $ (_h . _registers) $ run State
        { _program   = r
        , _pc        = 0
        , _num_mul   = 0
        , _registers = Registers 1 0 0 0 0 0 0 0
        }