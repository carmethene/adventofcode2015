import Data.List

flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (x:xs) = x ++ flatten xs

lookAndSay :: String -> String
lookAndSay val = flatten $ map (\x -> show (length x) ++ [head x]) (group val)

main = do
    input <- getLine
    let sq = input : [lookAndSay x | x <- sq]
    -- Part 1
    print $ "Length of #40: " ++ show (length $ sq !! 40)
    -- Part 2
    print $ "Length of #50: " ++ show (length $ sq !! 50)

