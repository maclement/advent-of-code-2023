import GHC.Arr
import Data.Char (isDigit, isSymbol)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List ((\\), nub)

main1 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve1 . input . lines

main2 :: IO ()
main2 = readFile "./input.txt"  >>= print .  solve2 . input . lines

input :: [String] -> Array (Int, Int) Char
input xs = array ((0,0), (length xs - 1, length (head xs) - 1)) $ concatMap go (zip [0 :: Int ..] (zip [0 :: Int ..] <$> xs))
 where
  go :: (Int, [(Int, Char)]) -> [((Int, Int), Char)]
  go (r, xs) = map (\(c, l) -> ((r, c), l)) xs

solve1 :: Array (Int, Int) Char -> Int
solve1 arr = sum . map snd . filter (not . null . fst) $ mapMaybe (numberAtPos arr) $ range (bounds arr)

numberAtPos :: Array (Int, Int) Char -> (Int, Int) -> Maybe ([(Int, Int)], Int)
numberAtPos arr ix@(x, y) | y == 0 || not (isDigit (arr ! (x, y - 1))) = second read <$> searchUntilFail arr ix
                          | otherwise = Nothing

searchUntilFail :: Array (Int, Int) Char -> (Int, Int) -> Maybe ([(Int, Int)], String)
searchUntilFail arr ix@(i, j) = isDigitPos arr ix  >>= \(b, c) -> maybe 
  (return (b, return c)) 
  (\(b',s) -> return (b ++ b', c : s)) 
  $ searchUntilFail arr (i, j + 1)

isDigitPos :: Array (Int, Int) Char -> (Int, Int) -> Maybe ([(Int, Int)], Char)
isDigitPos arr ix 
  | inRange (bounds arr) ix = 
    let c = arr ! ix 
    in if isDigit c 
         then Just (adjacentSym arr ix, c) 
         else Nothing 
  | otherwise               = Nothing

adjacentSym :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
adjacentSym arr (x,y) = 
            [ p 
            | x' <- [succ x, x, pred x]
            , y' <- [succ y, y, pred y]
            , (x, y) /= (x', y')
            , inRange (bounds arr) (x', y') 
            , Just p <- [isSymbolPos arr (x', y')]
            ]

isSymbolPos :: Array (Int, Int) Char -> (Int, Int) -> Maybe (Int, Int)
isSymbolPos arr ix | not (isDigit (arr ! ix) || (arr ! ix) == '.' ) = Just ix 
                   | otherwise                                      = Nothing

solve2 :: Array (Int, Int) Char -> Int
solve2 arr = go $ mapMaybe (numberAtPos arr) $ range (bounds arr)
 where
  go :: [([(Int, Int)], Int)] -> Int
  go xs = flip div 2 $ sum $ [ n1 * n2 
                             | (l1, n1) <- xs
                             , (l2, n2) <- xs
                             , (l1, n1) /= (l2, n2)
                             , not (null l1)
                             , not (null l2)
                             , null (nub l1 \\ nub l2) 
                             ]