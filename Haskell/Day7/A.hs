import Text.Parsec ( parse, Parsec, many1, alphaNum, char, many, digit, sepBy)
import Data.Functor ( ($>) )
import Data.Functor.Classes ( liftCompare, Ord1 (liftCompare) )
import Data.List (sortBy, sort, nub, sortBy, group, find, partition)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve (cmp value score)) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve (cmp valueJ scoreJ)) (parse inputP "" s)

type Parser = Parsec String ()

type Input = [Round]

inputP :: Parser Input
inputP = roundP `sepBy` char '\n'

data Round = Round { hand :: [Char], bid :: Int}
 deriving Show

roundP :: Parser Round
roundP = Round <$> many1 alphaNum <*> (spaces *> (read <$> many1 digit))

spaces :: Parser ()
spaces = many (char ' ') $> ()

cmp :: [(Char, Int)] -> ([Char] -> Int) -> Round -> Round -> Ordering
cmp m sc r1 r2 = liftCompare (liftCompare (compare `on` toIntG m)) (sc $ hand r1, hand r1) (sc $ hand r2, hand r2)

score :: [Char] -> Int
score cs = let (x:xs) = sortBy (flip compare `on` length) . group . sort $ cs
           in length x * 10 + if null xs then 0 else length (head xs)

toIntG :: [(Char, Int)] -> Char -> Int
toIntG m c = fromMaybe (read [c]) $ lookup c m 

solve :: (Round -> Round -> Ordering) -> [Round] -> Int
solve comp = sum . zipWith (*) [1..] . map bid . sortBy comp 

scoreJ :: [Char] -> Int
scoreJ cs = let (js, cs') = partition (=='J') cs 
                (x : xs)  = sortBy (flip compare `on` length) . group . sort $ cs'
            in if length js == 5 then 50 else (length x + length js) * 10 + if null xs then 0 else length (head xs)

valueJ, value :: [(Char, Int)]
value = [('T', 10), ('J', 11), ('Q', 12), ('K', 13), ('A', 14)]
valueJ = [('T', 10), ('J', 1), ('Q', 12), ('K', 13), ('A', 14)]