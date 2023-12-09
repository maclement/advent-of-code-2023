{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
import Text.Parsec ( Parsec, char, string, many1, many, parse, alphaNum, newline, sepBy, digit, (<|>))
import Data.Functor ( ($>) )
import Data.Bool ( bool )

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve2) (parse inputP "" s)

spaces :: Parser ()
spaces = many (char ' ') $> ()

type Parser = Parsec String ()

inputP :: Parser [[Integer]]
inputP =  lineP `sepBy` newline
 where
  lineP :: Parser [Integer]
  lineP = many1 (numberP <* spaces)

numberP :: Parser Integer
numberP = (char '-' *> ((`subtract` 0) . read <$> many1 digit)) <|> (read <$> many1 digit)

solve1, solve2 :: [[Integer]] -> Integer
solve1 = sum . map (helper True)
solve2 = sum . map (helper False)

helper :: Bool -> [Integer] -> Integer
helper b = foldr (flip (+) . last) 0 . takeWhile (any (/=0)) . iterate (\xs -> zipWith (-) (tail xs) xs) . bool reverse id b