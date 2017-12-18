{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (snd, mod, lookup, read)
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Map.Strict (Map, findWithDefault, insert, adjust, null)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, fromList, (!?))
import Data.Maybe

data Instruction = Snd Val
                 | Set Reg Val
                 | Add Reg Val
                 | Mul Reg Val
                 | Mod Reg Val
                 | Rcv Reg
                 | Jgz Reg Val deriving Show
type Reg = Char
data Val = RegVal Reg | IntVal Int deriving Show
type Program = Vector Instruction
type Registers = Map Reg Int
data State = State { _program :: Program
                   , _pc :: Int
                   , _lastSound :: Maybe Int
                   , _registers :: Registers } deriving Show

program :: GenParser Char st Program
program =
  fromList
    <$>     (snd <|> set <|> add <|> mul <|> mod <|> rcv <|> jgz)
    `sepBy` char '\n'

val :: GenParser Char st Val
val = IntVal <$> int <|> RegVal <$> anyChar

snd :: GenParser Char st Instruction
snd = Snd <$ try (string "snd ") <*> val

set :: GenParser Char st Instruction
set = Set <$ try (string "set ") <*> anyChar <* char ' ' <*> val

add :: GenParser Char st Instruction
add = Add <$ try (string "add ") <*> anyChar <* char ' ' <*> val

mul :: GenParser Char st Instruction
mul = Mul <$ try (string "mul ") <*> anyChar <* char ' ' <*> val

mod :: GenParser Char st Instruction
mod = Mod <$ try (string "mod ") <*> anyChar <* char ' ' <*> val

rcv :: GenParser Char st Instruction
rcv = Rcv <$ try (string "rcv ") <*> anyChar

jgz :: GenParser Char st Instruction
jgz = Jgz <$ try (string "jgz ") <*> anyChar <* char ' ' <*> val

read :: Val -> Registers -> Int
read (IntVal i) _         = i
read (RegVal r) registers = find r registers

find :: Reg -> Registers -> Int
find = findWithDefault 0

run :: State -> State
run state@State { _program, _pc, _lastSound, _registers } =
  case _program !? _pc of
    Just (Snd x) ->
      run state { _pc = _pc + 1, _lastSound = Just (read x _registers) }
    Just (Set x y) -> run state
      { _pc        = _pc + 1
      , _registers = insert x (read y _registers) _registers
      }
    Just (Add x y) -> run state
      { _pc        = _pc + 1
      , _registers = adjust (+read y _registers) x _registers
      }
    Just (Mul x y) -> run state
      { _pc        = _pc + 1
      , _registers = adjust (*read y _registers) x _registers
      }
    Just (Mod x y) -> run state
      { _pc        = _pc + 1
      , _registers = adjust (\x' -> x' `rem` (read y _registers)) x _registers
      }
    Just (Rcv x) ->
      if (find x _registers) == 0 then run state { _pc = _pc + 1 } else state
    Just (Jgz x y) -> run state
      { _pc = _pc + if (find x _registers) > 0 then read y _registers else 1
      }
    Nothing -> state

main :: IO ()
main = do
  content <- readFile "./18-input.txt"
  case parse program "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> print $ _lastSound $ run State
      { _program   = r
      , _pc        = 0
      , _lastSound = Nothing
      , _registers = Map.empty
      }