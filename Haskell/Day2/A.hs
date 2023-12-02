import Text.Parsec ( Parsec, digit, space, many1, string, (<|>), sepBy, char, optional, parse)
import Data.Functor (($>))
import Data.List (sort)

type Parser = Parsec String ()

type Input = [Game]

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve2) (parse inputP "" s)

test :: IO ()
test = readFile "./testinput.txt"  >>= \s -> either print (print . solve2) (parse inputP "" s)

inputP :: Parser Input
inputP = gameP `sepBy` char '\n'

data Game = Game {getId :: Int, getSets :: [Set]}
 deriving Show

gameP :: Parser Game
gameP = Game <$> (string "Game" *> space *> (read <$> many1 digit) <* char ':')
             <*> (space *> (setP `sepBy` string "; "))

data Set = Set {reds :: (Color, Int), greens :: (Color, Int), blues :: (Color, Int)}
 deriving Show

setP :: Parser Set
setP = convert <$> colorSetP `sepBy` string ", "
 where
  convert :: [(Color, Int)] -> Set
  convert []         = emptySet
  convert ((c,i):xs) = case c of
      Red   -> (convert xs) {reds = (Red, i)}
      Green -> (convert xs) {greens = (Green, i)}
      Blue  -> (convert xs) {blues = (Blue, i)}

colorSetP :: Parser (Color, Int)
colorSetP = flip (,) <$> (read <$> many1 digit) <* space <*> colorP

data Color = Red | Green | Blue
 deriving (Eq, Ord, Show)

colorP :: Parser Color
colorP = (string "blue" $> Blue) <|> (string "red" $> Red) <|> (string "green" $> Green)

solve1 :: Input -> Int
solve1 = sum . map getId . filter (not . violate)

emptySet :: Set
emptySet = Set (Red, 0) (Green, 0) (Blue, 0)

violate :: Game -> Bool
violate = any violateSet . getSets

violateSet :: Set -> Bool
violateSet s = snd (reds s) > 12
             || snd (greens s) > 13
             || snd (blues s) > 14

solve2 :: Input -> Int
solve2 = sum . map (multSet . combineGame)

combineGame :: Game -> Set
combineGame (Game i s) = helper s

helper :: [Set] -> Set
helper (s1:s2:xs) = s1 `combineMax` helper (s2 : xs)
helper []         = emptySet
helper [x]        = x

combineMax :: Set -> Set -> Set
combineMax (Set r g b) (Set r' g' b') = Set (max r r') (max g g') (max b b')

multSet :: Set -> Int
multSet (Set r g b) = product (filter (/= 0) [snd r,snd g, snd b])