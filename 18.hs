{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (snd, mod, read)
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Map.Strict (Map, findWithDefault, insert, adjust)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, fromList, (!?))
import Data.Sequence (Seq(Empty, (:<|)), (|>))
import qualified Data.Sequence as S

data Instruction = Snd Val
                 | Set Reg Val
                 | Add Reg Val
                 | Mul Reg Val
                 | Mod Reg Val
                 | Rcv Reg
                 | Jgz Val Val deriving Show
type Reg = Char
data Val = RegVal Reg | IntVal Int deriving Show
type Program = Vector Instruction
type Registers = Map Reg Int
data State = State { _program :: Program
                   , _pc :: Int
                   , _lastSound :: Maybe Int
                   , _registers :: Registers } deriving Show

type Queue = S.Seq Int
data Core = Core { _pc' :: Int
                 , _registers' :: !Registers
                 , _queue :: !Queue
                 , _timesSent :: Int
                 } deriving Show
data State' = State' { _program' :: Program
                     , _core1 :: !Core
                     , _core2 :: !Core
                     } deriving Show

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
jgz = Jgz <$ try (string "jgz ") <*> val <* char ' ' <*> val

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
      , _registers = adjust (+ read y _registers) x _registers
      }
    Just (Mul x y) -> run state
      { _pc        = _pc + 1
      , _registers = adjust (* read y _registers) x _registers
      }
    Just (Mod x y) -> run state
      { _pc        = _pc + 1
      , _registers = adjust (\x' -> x' `rem` read y _registers) x _registers
      }
    Just (Rcv x) ->
      if find x _registers == 0 then run state { _pc = _pc + 1 } else state
    Just (Jgz x y) -> run state
      { _pc = _pc + if read x _registers > 0 then read y _registers else 1
      }
    Nothing -> state

run' :: State' -> State'
run' state@(State' prog core1@(Core pc1 regs1 queue1 sent1) core2@(Core pc2 regs2 queue2 sent2))
  = let
      op (Set x y) (Core pc regs _ times) queue' =
        Core (pc + 1) (insert x (read y regs) regs) queue' times
      op (Add x y) (Core pc regs _ times) queue' =
        Core (pc + 1) (adjust (+ read y regs) x regs) queue' times
      op (Mul x y) (Core pc regs _ times) queue' =
        Core (pc + 1) (adjust (* read y regs) x regs) queue' times
      op (Mod x y) (Core pc regs _ times) queue' =
        Core (pc + 1) (adjust (\x' -> x' `rem` read y regs) x regs) queue' times
      op (Jgz x y) (Core pc regs _ times) queue' =
        Core (pc + if read x regs > 0 then read y regs else 1) regs queue' times
      op (Snd x) c q = error $ show x ++ show c ++ show q
      op (Rcv x) c q = error $ show x ++ show c ++ show q
    in
      case (prog !? pc1, prog !? pc2, queue1, queue2) of
        (Nothing     , Nothing     , _     , _    ) -> state
        (Just (Rcv _), Just (Rcv _), Empty , Empty) -> state
        (Just (Rcv _), Just (Rcv y), v:<|vs, Empty) -> run' $ state
          { _core1 = Core pc1 regs1 vs sent1
          , _core2 = Core (pc2 + 1) (insert y v regs2) queue2 sent2
          }
        (Just (Rcv x), Just (Rcv _), Empty, v:<|vs) -> run' $ state
          { _core1 = Core (pc1 + 1) (insert x v regs1) queue1 sent1
          , _core2 = Core pc2 regs2 vs sent2
          }
        (Just (Rcv x), Just (Rcv y), v:<|vs, t:<|ts) -> run' $ state
          { _core1 = Core (pc1 + 1) (insert x t regs1) vs sent1
          , _core2 = Core (pc2 + 1) (insert y v regs2) ts sent2
          }
        (Just (Snd x), Just (Rcv y), Empty, _) -> run' $ state
          { _core1 = Core (pc1 + 1) regs1 queue1 (sent1 + 1)
          , _core2 = Core (pc2 + 1) (insert y (read x regs1) regs2) queue2 sent2
          }
        (Just (Rcv x), Just (Snd y), _, Empty) -> run' $ state
          { _core1 = Core (pc1 + 1) (insert x (read y regs2) regs1) queue1 sent1
          , _core2 = Core (pc2 + 1) regs2 queue2 (sent2 + 1)
          }
        -- (Just (Snd x), Just (Rcv y), Empty, _) -> run' $ state
        --   { _core1 = Core (pc1 + 1) regs1 (S.singleton (read x regs1)) (sent1 + 1)
        --   , _core2 = core2
        --   }
        -- (Just (Rcv x), Just (Snd y), _, Empty) -> run' $ state
        --   { _core1 = core1
        --   , _core2 = Core (pc2 + 1) regs2 (S.singleton (read y regs2)) (sent2 + 1)
        --   }
        (Just (Snd x), Just (Rcv y), v:<|vs, _) -> run' $ state
          { _core1 = Core (pc1 + 1) regs1 (vs |> read x regs1) (sent1 + 1)
          , _core2 = Core (pc2 + 1) (insert y v regs2) queue2 sent2
          }
        (Just (Rcv x), Just (Snd y), _, v:<|vs) -> run' $ state
          { _core1 = Core (pc1 + 1) (insert x v regs1) queue1 sent1
          , _core2 = Core (pc2 + 1) regs2 (vs |> read y regs2) (sent2 + 1)
          }
        (Just (Snd x), Just (Snd y), _, _) -> run' $ state
          { _core1 = Core (pc1 + 1) regs1 (queue1 |> read x regs1) (sent1 + 1)
          , _core2 = Core (pc2 + 1) regs2 (queue2 |> read y regs2) (sent2 + 1)
          }
        (Just (Snd x), y, _, _) -> run' $ state
          { _core1 = Core (pc1 + 1) regs1 (queue1 |> read x regs1) (sent1 + 1)
          , _core2 = case y of
            Just y' -> op y' core2 queue2
            Nothing -> core2
          }
        (x, Just (Snd y), _, _) -> run' $ state
          { _core1 = case x of
            Just x' -> op x' core1 queue1
            Nothing -> core1
          , _core2 = Core (pc2 + 1) regs2 (queue2 |> read y regs2) (sent2 + 1)
          }
        (Just (Rcv _), Just y, _, Empty) ->
          run' $ state { _core1 = core1, _core2 = op y core2 queue2 }
        (Just x, Just (Rcv _), Empty, _) ->
          run' $ state { _core1 = op x core1 queue1, _core2 = core2 }
        (Just (Rcv x), Just y, _, v:<|vs) -> run' $ state
          { _core1 = Core (pc1 + 1) (insert x v regs1) queue1 sent1
          , _core2 = op y core2 vs
          }
        (Just x, Just (Rcv y), v:<|vs, _) -> run' $ state
          { _core1 = op x core1 vs
          , _core2 = Core (pc2 + 1) (insert y v regs2) queue2 sent2
          }
        (Just x, Just y, _, _) -> run'
          $ state { _core1 = op x core1 queue1, _core2 = op y core2 queue2 }
        _ -> error "ERROR"

initialState :: Program -> State
initialState r =
  State {_program = r, _pc = 0, _lastSound = Nothing, _registers = Map.empty}

initialState' :: Program -> State'
initialState' r = State' r
                         (Core 0 (Map.singleton 'p' 0) S.empty 0)
                         (Core 0 (Map.singleton 'p' 1) S.empty 0)

main :: IO ()
main = do
  content <- readFile "./18-input.txt"
  case parse program "(stdin)" content of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print $ _lastSound $ run (initialState r)
      print
        $ (_timesSent . _core2)
        $ run' (initialState' r)