import Data.Map ( Map )
import qualified Data.Map as M
import Control.Monad.State ( gets, evalState, State, modify)
import Data.Maybe ( fromMaybe, isJust, fromJust, listToMaybe )
import Text.Parsec ( Parsec, many1, digit, char, sepBy, oneOf, space, parse, newline )
import Data.Bifunctor (bimap)
import Data.List (intercalate)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= either print (print . solve1) . parse inputP ""

main2 :: IO ()
main2 = readFile "./input.txt"  >>= either print (print . solve1 . map (bimap (intercalate "?" . replicate 5) (concat . replicate 5))) . parse inputP ""

type Parser = Parsec String ()

inputP :: Parser [(String, [Int])]
inputP = lineP `sepBy` newline

lineP :: Parser (String, [Int])
lineP = (,) <$> many1 (oneOf "?#.") <* space <*> (read <$> many1 digit) `sepBy` char ','

solve1 :: [(String, [Int])] -> Int
solve1 = sum . map run 

run :: (String, [Int]) -> Int
run (s, is) = evalState (memorize s is) M.empty

-- TODO: Refactor
memorize :: String -> [Int] -> State (Map (String, [Int]) Int) Int
memorize s  [] | '#' `elem` s = return 0
               | otherwise    = return 1
memorize [] _  = return 0
memorize s i@(x:xs) = do
    mi <- gets (M.lookup (s, i))
    maybe (case head s of
        '?' -> write $ (+) <$> memorize ('#' : tail s) i <*> memorize ('.' : tail s) i
        '#' | x <= length s && '.' `notElem` take x s -> let l = drop x s in case listToMaybe l of
                                                           Just '#' -> return 0
                                                           _        -> write $ memorize (drop 1 l) xs
            | otherwise                               -> return 0
        '.' -> write $ memorize (tail s) (x : xs)) return mi
 where
  write :: State (Map (String, [Int]) Int) Int -> State (Map (String, [Int]) Int) Int
  write a = a >>= \res -> modify (M.insert (s,i) res) >> return res