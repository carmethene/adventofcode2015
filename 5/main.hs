import Data.List

countElements :: Eq a => [a] -> [a] -> Int
countElements e xs = length $ filter (`elem` e) xs

hasDoubleElement :: Eq a => [a] -> Bool
hasDoubleElement (x:y:xys) = x == y || hasDoubleElement (y:xys)
hasDoubleElement _ = False

hasInfix :: Eq a => [[a]] -> [a] -> Bool
hasInfix (x:xs) ys = isInfixOf x ys || hasInfix xs ys
hasInfix _ _ = False

isNice1 :: String -> Bool
isNice1 str =
    countElements "aeiou" str >= 3 &&
    hasDoubleElement str &&
    not (hasInfix ["ab", "cd", "pq", "xy"] str)

hasRepeatingPair :: Eq a => [a] -> Bool
hasRepeatingPair (x:y:xys) = isInfixOf [x, y] xys || hasRepeatingPair (y:xys)
hasRepeatingPair _ = False

hasSpacedRepeat :: Eq a => [a] -> Bool
hasSpacedRepeat (x:y:z:xyzs) = x == z || hasSpacedRepeat (y:z:xyzs)
hasSpacedRepeat _ = False

isNice2 :: String -> Bool
isNice2 str =
    hasRepeatingPair str &&
    hasSpacedRepeat str

main = do
    input <- getContents
    let strings = lines input
    print $ "Number of nice strings (1): " ++ show (length $ filter isNice1 strings)
    print $ "Number of nice strings (2): " ++ show (length $ filter isNice2 strings)

