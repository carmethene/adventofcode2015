import Data.List

countElements :: Eq a => [a] -> [a] -> Int
countElements e xs = length $ filter (`elem` e) xs

hasDoubleElement :: Eq a => [a] -> Bool
hasDoubleElement (x:y:xys) = x == y || hasDoubleElement (y:xys)
hasDoubleElement _ = False

hasInfix :: Eq a => [[a]] -> [a] -> Bool
hasInfix (x:xs) ys = isInfixOf x ys || hasInfix xs ys
hasInfix _ _ = False

isNice :: String -> Bool
isNice str =
    countElements "aeiou" str >= 3 &&
    hasDoubleElement str &&
    not (hasInfix ["ab", "cd", "pq", "xy"] str)

main = do
    input <- getContents
    let strings = lines input
    print $ "Number of nice strings: " ++ show (length $ filter isNice strings)

