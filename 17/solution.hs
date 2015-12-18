import Data.List

combinations :: Int -> [Int] -> [[Int]]
combinations target [] = []
combinations target (x:xs)
    | x >  target = []
    | x == target = [x] : restWithoutSelf
    | x <  target = restWithSelf ++ restWithoutSelf where
        restWithSelf    = map ([x] ++) $ combinations (target - x) xs
        restWithoutSelf = combinations target xs

main = do
    input <- readFile "input.txt"
    let containers = sort $ map (read :: String -> Int) $ lines input
    print $ "Number of combinations: " ++ show (length (combinations 150 containers))

