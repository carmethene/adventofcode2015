import Data.List

combinationsOfSize :: Int -> [Int] -> [[Int]]
combinationsOfSize target [] = []
combinationsOfSize target (x:xs)
    | x >  target = []
    | x == target = [x] : restWithoutSelf
    | x <  target = restWithSelf ++ restWithoutSelf where
        restWithSelf    = map ([x] ++) $ combinationsOfSize (target - x) xs
        restWithoutSelf = combinationsOfSize target xs

main = do
    input <- readFile "input.txt"
    let containers   = sort $ map (read :: String -> Int) $ lines input
    let combinations = combinationsOfSize 150 containers
    -- Part 1
    print $ "Number of combinations: " ++ show (length combinations)
    -- Part 2
    let minLength = minimum (map length combinations)
    let minCombinations = filter ((== minLength) . length) combinations
    print $ "Minimum combinations:   " ++ show (length minCombinations)

