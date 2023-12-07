import Text.Parsec ( parse, Parsec, many1, alphaNum, char, many, digit, sepBy)
import Data.Functor ( ($>) )
import Data.List (sortBy, sort, nub, sortBy, group, find, partition)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve cmp) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve cmpJ) (parse inputP "" s)

type Parser = Parsec String ()

type Input = [Round]

inputP :: Parser Input
inputP = roundP `sepBy` char '\n'

data Round = Round { hand :: [Char], bid :: Int, modified :: [Char] }
 deriving Show

roundP :: Parser Round
roundP = (\h b -> Round h b (sort $ nub h)) <$> many1 alphaNum <*> (spaces *> (read <$> many1 digit))

spaces :: Parser ()
spaces = many (char ' ') $> ()

cmp :: Round -> Round -> Ordering
cmp r1 r2 = case compare (score $ hand r1) (score $ hand r2) of
    EQ -> head $ filter (/= EQ) (zipWith (cmpLetter toInt) (hand r1) (hand r2))
    x  -> x

score :: [Char] -> Int
score cs = let (x:xs) = sortBy (flip compare `on` length) . group . sort $ cs
           in length x * 10 + if null xs then 0 else length (head xs)

cmpLetter :: (Char -> Int) -> Char -> Char -> Ordering
cmpLetter f c1 c2 = f c1 `compare` f c2

toInt :: Char -> Int
toInt c = fromMaybe (read [c]) $ lookup c [('T', 10), ('J', 11), ('Q', 12), ('K', 13), ('A', 14)]

solve :: (Round -> Round -> Ordering) -> [Round] -> Int
solve comp = sum . zipWith (*) [1..] . map bid . sortBy comp 

cmpJ :: Round -> Round -> Ordering
cmpJ r1 r2 = case compare (scoreJ $ hand r1) (scoreJ $ hand r2) of
    EQ -> head $ filter (/= EQ) (zipWith (cmpLetter toIntJoker) (hand r1) (hand r2))
    x  -> x

scoreJ :: [Char] -> Int
scoreJ cs = let (js, cs') = partition (=='J') cs 
                (x : xs)  = sortBy (flip compare `on` length) . group . sort $ cs'
            in if length js == 5 then 50 else (length x + length js) * 10 + if null xs then 0 else length (head xs)

toIntJoker :: Char -> Int
toIntJoker c = fromMaybe (read [c]) $ lookup c [('T', 10), ('J', 1), ('Q', 12), ('K', 13), ('A', 14)]