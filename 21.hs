import Prelude hiding (lookup)
import Data.List.Extra hiding (lookup)
import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict hiding (toList, foldl')
import Data.Matrix hiding (fromList, matrix)
import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as P

data Pixel = On | Off deriving (Show, Eq)
data Rule = Rule (Matrix Pixel) (Matrix Pixel) deriving Show
type Rules = (HashMap (Matrix Pixel) (Matrix Pixel))

instance Hashable Pixel where
  hashWithSalt salt On = hashWithSalt salt True
  hashWithSalt salt Off = hashWithSalt salt False

instance (Hashable a) => Hashable (Matrix a) where
  hashWithSalt salt matrix = hashWithSalt salt $ toList matrix -- does it fuse?

pixel :: GenParser Char st Pixel
pixel = On <$ char '#' P.<|> Off <$ char '.'


rule :: GenParser Char st (Matrix Pixel, Matrix Pixel)
rule = do
  lhs <- many pixel `sepBy` char '/'
  _   <- string " => "
  rhs <- many pixel `sepBy` char '/'
  return $ (fromLists lhs, fromLists rhs)

rulesParser :: GenParser Char st Rules
rulesParser = do
  xs <- rule `sepBy` char '\n'
  return $ fromList (xs >>= permute)

initial :: Matrix Pixel
initial = fromLists [[Off, On, Off], [Off, Off, On], [On, On, On]]

step :: Rules -> Matrix Pixel -> Matrix Pixel
step rules matrix = if ncols matrix `rem` 2 == 0
  then step' 2 rules matrix -- break into twos, enhance
  else step' 3 rules matrix -- break into threes, enhance

step' :: Int -> Rules -> Matrix Pixel -> Matrix Pixel
step' n rules matrix =
  let cols = ncols matrix
      rows = nrows matrix
      f    = step' n rules
  in  if cols == n
        then if rows == n
          then fromJust $ lookup matrix rules -- head [a | Just a <- fmap (\f -> lookup (f matrix) rules) transformations]
          else
            let hd = submatrix 1 n 1 n matrix
                tl = submatrix (n + 1) rows 1 n matrix
            in  (f hd) <-> (f tl) -- destructure rows
        else if rows == n
          then
            let hd = submatrix 1 n 1 n matrix
                tl = submatrix 1 n (n + 1) cols matrix
            in  (f hd) <|> (f tl) -- destructure cols
          else
            let (tl, tr, bl, br) = splitBlocks n n matrix
            in  (f tl <|> f tr) <-> (f bl <|> f br) -- destructure both

-- rot right
-- transpose
rot :: Matrix a -> Matrix a
rot matrix = case toLists matrix of
  [[a, b], [c, d]] -> fromLists [[c, a], [d, b]]
  [[a, b, c], [d, e, f], [g, h, i]] ->
    fromLists [[g, d, a], [h, e, b], [i, f, c]]

-- flip horizontally
-- multiply with something?
flip' :: Matrix a -> Matrix a
flip' matrix = case toLists matrix of
  [[a, b], [c, d]] -> fromLists [[b, a], [d, c]]
  [[a, b, c], [d, e, f], [g, h, i]] ->
    fromLists [[c, b, a], [f, e, d], [i, h, g]]

permute :: (Matrix a, Matrix a) -> [(Matrix a, Matrix a)]
permute (a, b) =
  [ (a                          , b)
  , (rot a                      , b)
  , ((rot . rot) a              , b)
  , ((rot . rot . rot) a        , b)
  , (flip' a                    , b)
  , ((flip' . rot) a            , b)
  , ((flip' . rot . rot) a      , b)
  , ((flip' . rot . rot . rot) a, b)
  ]

toInt :: Num n => Pixel -> n
toInt On  = 1
toInt Off = 0

main :: IO ()
main = do
  contents <- readFile "./21-input.txt"
  case parse rulesParser "(stdin)" contents of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right rules -> do
      -- print rules
      -- let matrix = iterate (step rules) initial !! 5
      -- let count  = foldl' (\acc n -> toInt n + acc) 0 (toList matrix)
      -- print count
      let matrix' = iterate (step rules) initial !! 15 -- 18
      let count'  = foldl' (\acc n -> toInt n + acc) 0 (toList matrix')
      print count'