import System.IO
import Data.List

-- calculateSimilarity::[Int]->[Int]->Int
calculateSimilarity xs ys = sum (map ((\(y,z)->y*length z) . (\x-> (x, filter (==x) ys ))) xs)
main::IO()
main = do
        let filename = "input.txt"
        contents <- readFile filename
        let linesOfFile = lines contents
        let wordsOfLine = map (map (\x -> read x ::Int).words) linesOfFile
        let (first,second) = foldr (\xs (ys,zs)-> (head xs:ys, head (tail xs) :zs) ) ([],[]) wordsOfLine
        let pairs = [(x,y)|(x,y)<- zip (sort first) (sort second)]
        let absvals = map (\(x,y)-> abs (x-y)) pairs
        let totalDistance = sum absvals
        let similarity = calculateSimilarity first second
        print totalDistance
        print similarity


