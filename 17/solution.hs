import Data.List

combinationsOfSize :: Int -> [Int] -> [[Int]]
combinationsOfSize target [] = []
combinationsOfSize target (x:xs)
    | x >  target = [] -- If (x:xs) is unsorted, use restWithoutSelf instead
    | x == target = [x] : restWithoutSelf
    | x <  target = restWithSelf ++ restWithoutSelf where
        restWithSelf    = map ([x] ++) $ combinationsOfSize (target - x) xs
        restWithoutSelf = combinationsOfSize target xs

main = do
    input <- getContents
    let containers   = sort $ map (read :: String -> Int) $ lines input
    let combinations = combinationsOfSize 150 containers
    -- Part 1
    print $ "Num combinations: " ++ show (length combinations)
    -- Part 2
    let minLength = minimum (map length combinations)
    let minCombinations = filter ((== minLength) . length) combinations
    print $ "Min combinations: " ++ show (length minCombinations)

