import Text.Parsec
    ( Parsec, char, digit, space, spaces, string, many1, sepBy, parse, parseTest, sepBy1, endOfLine, oneOf)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve2) (parse inputP "" s)

type Parser = Parsec String ()

inputP :: Parser [Card]
inputP = cardP `sepBy` endOfLine

data Card = Card  { cid :: Int, res :: [Int], win :: [Int], amount :: Int}
 deriving Show

cardP :: Parser Card
cardP = Card <$> (string "Card" *> spaces *> (read <$> many1 digit) <* char ':' <* spaces) 
             <*> many1 (read <$> (many1 digit <* spaces))
             <*> (char '|' *> many1 (read <$> (many1 (oneOf " ") *> many1 digit)))
             <*> pure 1

solve1 :: [Card] -> Int
solve1 = sum . map (toPower . computeWin)
 where
  toPower :: Int -> Int
  toPower 0 = 0
  toPower n = 2^(n-1)

computeWin :: Card -> Int
computeWin c = length $ filter (`elem` win c) (res c) 

solve2 :: [Card] -> Int
solve2 = sum . map amount . iterateCards

iterateCards :: [Card] -> [Card]
iterateCards []     = []
iterateCards (c:cs) = c : iterateCards (incrementNext (computeWin c) (amount c) cs)
 
incrementNext :: Int -> Int -> [Card] -> [Card]
incrementNext 0 _ cs = cs
incrementNext _ _ [] = []
incrementNext n extra ((Card i r w a) : cs) = Card i r w (a + extra) : incrementNext (n - 1) extra cs