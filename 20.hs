import Data.List
import Data.Maybe
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

data Vec = Vec { _x :: Int, _y :: Int, _z :: Int } deriving Show
data Point = Point { _a :: Vec, _v :: Vec, _p :: Vec, _i :: Int } deriving Show

-- instance Monoid Vec where
--   mempty = Vec 0 0 0
--   mappend vec1 vec2 = Vec (_x vec1 + _x vec2) (_y vec1 + _y vec2) (_z vec1 + _z vec2)

instance Eq Vec where
  (==) v1 v2 = _x v1 == _x v2 && _y v1 == _y v2 && _z v1 == _z v2

-- instance Ord Vec where
--   compare v1 v2 = compare (dist v1) (dist v2)

instance Hashable Vec where
  hashWithSalt salt (Vec x y z) = salt `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

dist :: Vec -> Int
dist vec = abs (_x vec) + abs (_y vec) + abs (_z vec)

vector :: GenParser Char st Vec
vector = do
  _ <- char '<'
  x <- int
  _ <- char ','
  y <- int
  _ <- char ','
  z <- int
  _ <- char '>'
  return $ Vec x y z

point :: GenParser Char st (Vec, Vec, Vec)
point = do
  _ <- string "p="
  p <- vector
  _ <- string ", v="
  v <- vector
  _ <- string ", a="
  a <- vector
  return (a, v, p)

points :: GenParser Char st [(Vec, Vec, Vec)]
points = point `sepBy` char '\n'

-- delta :: Num n => (n, n, n) -> n
-- delta (a, b, c) = b * b - 4 * a * c

-- solutions :: RealFloat n => (n, n, n) -> [n]
-- solutions (a, b, c) = nub $ filter
--   (not . isNaN)
--   [ (-b - (sqrt (delta (a, b, c)))) / (2 * a)
--   , (-b + (sqrt (delta (a, b, c)))) / (2 * a)
--   ]

-- collisionTimes :: (Num b, Applicative f) => f b -> f b -> f b
-- collisionTimes p1 p2 = solutions ((-) <$> p1 <*> p2)

runVec :: Vec -> Vec -> Vec
runVec (Vec x y z) (Vec x' y' z') = Vec {_x = x + x', _y = y + y', _z = z + z'}

runPoint :: Point -> Point
runPoint (Point a v p i) = Point {_a = a, _v = v', _p = runVec v' p, _i = i}
  where v' = runVec a v

run :: [Point] -> [Point]
run ps =
  let newPoints = map runPoint ps
      grouped   = foldl' (\m p -> insert' (_p p) p m) M.empty newPoints
      filtered  = M.filter (\l -> length l == 1) grouped
      elems     = M.elems filtered
  in  head <$> elems

insert' :: Vec -> Point -> HashMap Vec [Point] -> HashMap Vec [Point]
insert' k v m = M.insert k val m where val = v : fromMaybe [] (M.lookup k m)

main :: IO ()
main = do
  contents <- readFile "./20-input.txt"
  case parse points "(stdin)" contents of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      let ps = zipWith (\(x, y, z) i -> Point x y z i) r [0 ..]
      print $ take 10 $ sortOn (dist . _a) ps
      print $ length $ iterate run ps !! 1000