import System.IO
import Data.List


isinLimits ::  Int -> Int -> Bool
isinLimits x y = (abs (x-y) >= 1) && (abs (x-y) <=3)

isIncreasingAndInLimits::[Int] -> Bool
isIncreasingAndInLimits [x] = True
isIncreasingAndInLimits (x:xs)  = (x < head xs) && isinLimits x (head xs) && isIncreasingAndInLimits xs

isDecreasingAndInLimits::[Int]-> Bool
isDecreasingAndInLimits [x] = True
isDecreasingAndInLimits (x:xs) = (x > head xs) && isinLimits x (head xs) && isDecreasingAndInLimits xs

isSafe::[Int] -> Bool
isSafe xs = isDecreasingAndInLimits xs ||isIncreasingAndInLimits xs

removeAt :: [a] -> Int -> [a]
removeAt xs n = take n xs ++ drop (n+1) xs

isSafe2 :: [Int] -> Bool
isSafe2 xs = any (isSafe.removeAt xs) [0..(length xs -1)]

main::IO()
main = do
        let filename = "input.txt"
        contents <- readFile filename
        let linesOfFile = lines contents
        let wordsOfLine = map (map (\x -> read x ::Int).words) linesOfFile
        let part1 = filter isSafe wordsOfLine
        let part2 = filter isSafe2 wordsOfLine
        print (length part1)
        print (length part2)
      





