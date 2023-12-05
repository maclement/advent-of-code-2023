import Text.Parsec (Parsec, newline, string, char, many, letter, many1, digit, sepBy, parse, parseTest, optional)
import Data.Functor (($>))
import Data.Maybe ( listToMaybe, fromMaybe, mapMaybe )
import Data.List ( sort )
import Data.Bifunctor (first)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= \s -> either print (print . solve1) (parse inputP "" s)

main2 :: IO ()
main2 = readFile "./input.txt"  >>= \s -> either print (print . solve2 . first intervals) (parse inputP "" s)

type Parser = Parsec String ()

type Input = ([Int], [Category])

inputP :: Parser Input
inputP = (,) <$> seedsP <* newline <*> (categoryP `sepBy` newline)

seedsP :: Parser [Int]
seedsP = string "seeds:" *> many1 (spaces *> (read <$> many1 digit)) <* newline

type Category = [Entry]

categoryP :: Parser Category
categoryP = nameP *> many1 (entryP <* optional newline)

data Entry = Entry { destRange, srcRange, rangeLength :: Int }
    deriving Show

spaces :: Parser ()
spaces = many1 (char ' ') $> ()

entryP :: Parser Entry
entryP = Entry <$> (read <$> many1 digit) <* spaces <*> (read <$> many1 digit) <* spaces <*> (read <$> many1 digit)

nameP :: Parser ()
nameP = many letter *> string "-to-" *> many letter *> string " map:" *> char '\n'$> ()

solve1 :: Input -> Int
solve1 = minimum . uncurry unrollMaps
 where
  unrollMaps :: [Int] -> [Category] -> [Int]
  unrollMaps acc []     = acc
  unrollMaps acc (c:cs) = unrollMaps (map (apply c) acc) cs

apply :: Category -> Int -> Int
apply es i = fromMaybe i $ listToMaybe $ mapMaybe (inBound i) es

inBound :: Int -> Entry -> Maybe Int
inBound i e | i >= srcRange e && i <= srcRange e + rangeLength e
            = let n = i - srcRange e in Just $ destRange e + n
            | otherwise = Nothing

type Interval = (Int, Int) 

intervals :: [Int] -> [Interval]
intervals []      = []
intervals (x:y:z) = (x, x + y - 1) : intervals z

-- invariant: intervals are sorted
combine :: [Interval] -> [Interval]
combine ((i, j):(a, b):xs)
  | j + 1 == a = combine ((i, b):xs)
  | a <= j    = combine ((i, max j b):xs)
  | otherwise = (i, j):combine ((a, b):xs)
combine is = is

applyI :: Category -> [Interval] -> [Interval]
applyI _  []     = []
applyI es (i:is) = case applyC es i of
    (Just r, more) -> r : applyI es (more ++ is)
    (Nothing,   r) -> r ++ applyI es is 

applyC :: [Entry] -> Interval -> (Maybe Interval, [Interval])
applyC []       i = (Nothing, [i])
applyC (e : es) i = case inBoundI i e of
    s@(Just _, _) -> s
    (Nothing,  _) -> applyC es i 

inBoundI :: Interval -> Entry -> (Maybe Interval, [Interval])
inBoundI (x, y) e = 
    let mxl = max x (srcRange e)
        mnr = min y (srcRange e + rangeLength e - 1)
    in if mxl < mnr 
        then (Just $ trans (mxl, mnr) e, filter (uncurry (<)) [(x, mxl - 1), (mnr + 1, y)])
        else (Nothing, [(x, y)])

trans :: Interval -> Entry -> Interval
trans (mxl, mnr) e = let d = destRange e - srcRange e
                     in (mxl + d, mnr + d)

inRange :: Int -> Entry -> Bool
inRange i e = i >= srcRange e && i <= srcRange e + rangeLength e

-- 
solve2 :: ([Interval], [Category]) -> Int
solve2 (is, []    ) = minimum $ map fst is  
solve2 (is, c : cs) = solve2 (applyI c $ combine $ sort is, cs)  