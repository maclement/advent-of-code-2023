import Text.Parsec ( Parsec, char, string, many1, many, parse, alphaNum, newline, sepBy, letter)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bool (bool)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . uncurry solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . uncurry solve2) (parse inputP "" s)

spaces :: Parser ()
spaces = many (char ' ') $> ()

type Parser = Parsec String ()

inputP :: Parser (String, Map String (String, String))
inputP =  (\x y -> (x, M.fromList y)) <$> stringP <* newline <* newline <*> entryP `sepBy` newline

type Entry = (String, (String, String))

entryP :: Parser Entry
entryP = (\x y z -> (x, (y, z))) <$> (stringP <* string " = ") <*> (char '(' *> stringP <* string ", ") <*> (stringP <* char ')') 

stringP :: Parser String
stringP = many1 letter

solve1, solve2 :: String ->  Map String (String, String) -> Int
solve1 s m = go (cycle s) 0 m "AAA"
solve2 s m = foldr1 lcm $ map (go (cycle s) 0 m) (filter (('A' ==) . last) (M.keys m))  

go :: String -> Int -> Map String (String, String) -> String -> Int
go (x:xs) acc m s | last s == 'Z' = acc
                  | otherwise     = go xs (acc + 1) m (bool fst snd (x == 'R') (m M.! s))

