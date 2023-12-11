main1 :: IO ()
main1 = readFile "./input.txt"  >>= print . solve 2 . computeEmpty . input . lines

main2 :: IO ()
main2 = readFile "./input.txt"  >>= print .  solve 1000000 . computeEmpty . input . lines

data Input = Input { boundX, boundY :: Int, galaxies :: [(Int, Int)], empty :: ([Int], [Int]) }

input :: [String] -> Input
input xs = let zipped = zip [0..] (zip [0..] <$> xs)
           in Input (length xs - 1) (length (head xs) - 1) (concatMap go zipped) ([], [])
 where
  go :: (Int, [(Int, Char)]) -> [(Int, Int)]
  go (r, xs) = [ (r, l) | (l, c) <- xs, c == '#' ]

computeEmpty :: Input -> Input
computeEmpty i = i { empty = (emptyLine (boundX i) fst, emptyLine (boundY i) snd) }
 where
  emptyLine bound selector = [ x | x <- [0..bound], x `notElem` map selector (galaxies i) ]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distSpace :: ([Int], [Int]) -> (Int, Int) -> (Int, Int) -> Int
distSpace (emptyX, emptyY) (x1, y1) (x2, y2) = let f a b l = length (filter (`elem` [min a b .. max a b]) l)
                                               in f x1 x2 emptyX + f y1 y2 emptyY  

solve :: Int -> Input -> Int
solve m g = sum [ pred m * distSpace (empty g) g1 g2 + dist g1 g2 | g1 <- galaxies g, g2 <- galaxies g, g1 /= g2 ] `div` 2