import Data.Maybe (isJust, isNothing, mapMaybe)
import Control.Applicative ((<|>))
import Data.List (transpose)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . sum . mapMaybe (findMirrors isVerticalMirror) . splitWith null . lines
main2 = readFile "./input.txt"  >>= print . sum . mapMaybe (findMirrors isVerticalWithSmudge) . splitWith null . lines


-- | Splits a list at every point where the given predicate is true
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p [] = []
splitWith p xs = let x = dropWhile (not . p) xs
                 in takeWhile (not . p) xs : if null x then [] else splitWith p (tail x)

-- | Finds the symmetry in a given piece via the giving checking function
--   By definition each set has exactly one symmetry w.r.t the checking function
findMirrors :: ([String] -> Maybe Int) -> [String] -> Maybe Int
findMirrors check s = do
    let t = transpose s
    -- vertical checks (left to right then right to left)
    let verticals   = ((length $ head s) -) <$> iterateVerticalMirror check s <|> iterateVerticalMirror check (map reverse s) 
    -- horizontal checks (top to bottom then bottom to top)
    let horizontals = (length s -) <$> iterateVerticalMirror check t <|> iterateVerticalMirror check (map reverse t)
    verticals <|> (100 *) <$> horizontals

-- | Iteratively searches palindrom that includes right edge
iterateVerticalMirror :: ([String] -> Maybe Int) -> [String] -> Maybe Int
iterateVerticalMirror check ([]:_) = Nothing
iterateVerticalMirror check s      | isJust $ check s = check s
                                   | otherwise        = iterateVerticalMirror check $ map tail s

-- | Checks if all strings contain a palindrome that spans to the right
isVerticalMirror :: [String] -> Maybe Int
isVerticalMirror s = head <$> mapM isEvenPalindrome s

-- | Checks if a given string is an palindrome of even length
isEvenPalindrome :: String -> Maybe Int
isEvenPalindrome s | length s < 2                      = Nothing
                   | s == reverse s && even (length s) = Just $ length s `div` 2
                   | otherwise                         = Nothing

-- | If a singular line is not a palindrome we check if we can fix it (creating a new mirror) by changing exactly one piece
isVerticalWithSmudge :: [String] -> Maybe Int
isVerticalWithSmudge s = do
  let checks  = map isEvenPalindrome s
  let results = zip checks s
  let misfits = filter (isNothing . fst) results
  if length misfits == 1 && (canFix . snd . head) misfits
    then head (filter isJust checks) 
    else Nothing

-- | Checks if a given string is an even palindrom after exactly one piece was flipped
canFix :: String -> Bool
canFix s = isJust $ foldr1 (<|>) [ isEvenPalindrome (flipAt i s) | i <- [0..(length s - 1)] ]
 where
  flipAt n xs = let (y:ys) = drop (n - 1) xs
                 in  take (n - 1) xs  ++ flip' y : ys 

  flip' '.' = '#'
  flip' '#' = '.'