import System.IO
import Data.List
import Text.Read (readMaybe)
data Mul = Mul Int Int
mulKeyWord = "mul"
leftParen = '('
rightParen = ')'
comma =','

dontKeyWord = "don't()"
doKeyWord = "do"


getNumbers :: String -> (Int, Int)
getNumbers [] = (0, 0)
getNumbers xs = 
    case (readMaybe firstPart, readMaybe secondPart) of
        (Just n1, Just n2) -> (n1, n2)
        _                  -> (0, 0)
  where
    comma = ','
    rightParen = ')'
    firstPart = takeWhile (/= comma) xs
    secondPart = takeWhile (/= rightParen) . drop 1 . dropWhile (/= comma) $ xs



parseData [] ys = ys
parseData (a:b:c:d:xs) ys = if a:b:[c] == mulKeyWord && d == leftParen 
                                    then parseData xs ((Mul (fst (getNumbers xs)) (snd (getNumbers xs))) : ys)
                                    else parseData (b:c:d:xs) ys
parseData (_:xs) ys = parseData xs ys

eval :: Mul -> Int
eval (Mul m n) = m * n
main::IO()
main = do
        let filename = "input.txt"
        contents <- readFile filename
        let linesOfFile = lines contents
        let mulResults = concatMap (\x->parseData x []) linesOfFile
        let sumMul = sum (map eval mulResults)
        print (sumMul)
        
