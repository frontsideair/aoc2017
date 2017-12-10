import Text.ParserCombinators.Parsec

data Stream = Group [Stream] | Garbage String deriving Show

-- "{{},<>}" => Group [Group [], Garbage ""]
group :: GenParser Char st Stream
group = do
  _ <- char '{'
  g <- (group <|> garbage) `sepBy` char ','
  _ <- char '}'
  return $ Group g

-- "<bla<<bla>" -> Gargabe "blabla"
garbage :: GenParser Char st Stream
garbage = do
  _ <- char '<'
  g <- many $ noneOf ">!" `sepBy` exclamation
  _ <- char '>'
  return $ Garbage $ concat g

exclamation :: GenParser Char st ()
exclamation = char '!' >> anyChar >> return ()

count1 :: Int -> Stream -> Int
count1 n (Group xs) = foldl (\acc x -> acc + count1 (n + 1) x) (n + 1) xs
count1 _ _          = 0

count2 :: Int -> Stream -> Int
count2 n (Group   xs) = foldl (\acc x -> acc + count2 n x) 0 xs
count2 n (Garbage s ) = n + length s

main :: IO ()
main = do
  c <- getContents
  case parse group "(stdin)" c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> do
      print $ count1 0 r
      print $ count2 0 r

-- main :: IO ()
-- main = do
--   contents <- readFile "9-input.txt"
--   print (parse group "(unknown)" contents)

-- main :: IO ()
-- main = do
--   print (parse group "(unknown)" "{}")
--   print (parse group "(unknown)" "{<>}")
--   print (parse group "(unknown)" "{<bla>}")
--   print (parse group "(unknown)" "{<!!bla>}")
--   print (parse group "(unknown)" "{<!!bla!!>}")