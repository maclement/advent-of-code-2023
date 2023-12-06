import Text.Parsec ( Parsec, char, string, many1, many, parse, digit)
import Data.Functor (($>))

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . naiv) (parse inputP2 "" s)

type Parser = Parsec String ()

spaces :: Parser ()
spaces = many (char ' ') $> ()

inputP :: Parser [(Int, Int)]
inputP = zip <$> (string "Time:" *> intsP <* char '\n') <*> (string "Distance:" *> intsP)

intsP :: Parser [Int]
intsP = many1 (spaces *> (read <$> many1 digit))

solve1 :: [(Int, Int)] -> Int
solve1 = product . map naiv

naiv :: (Int, Int) -> Int
naiv (time, dist) = length [ x | x <- [0..time], let deltaTime = time - x, dist < deltaTime * x]

inputP2 :: Parser (Int, Int)
inputP2 = (,) <$> (string "Time:" *> intsP2 <* char '\n') <*> (string "Distance:" *> intsP2)

intsP2 :: Parser Int
intsP2 = read . concat <$> many (spaces *> many1 digit)