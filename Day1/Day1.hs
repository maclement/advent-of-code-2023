import Data.Char (isNumber)
import Data.List ( isPrefixOf )
import Data.Maybe (listToMaybe)

main1, main2 :: IO ()
main1 = readFile "./input.txt"  >>= print . sum . map (solve id) . lines
main2 = readFile "./input.txt"  >>= print . sum . map (solve replace) . lines

solve :: (String -> String) -> String -> Int
solve f s = let nums = filter isNumber (f s)
            in read [head nums, last nums]

replacements :: [(String, Char)]
replacements = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

replace :: String -> String
replace [] = []
replace s@(x:xs) = maybe x snd (listToMaybe [ p | p <- replacements, fst p `isPrefixOf` s]) : replace xs

test1 :: [String]
test1 = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

test2 :: [String] 
test2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four" ,"4nineeightseven2" ,"zoneight234", "7pqrstsixteen"]