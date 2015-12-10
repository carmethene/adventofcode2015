import Data.List

lookAndSay :: String -> String
lookAndSay val = foldr (\x y -> show (length x) ++ [head x] ++ y) "" (group val)

main = do
    input <- getLine
    let sq = input : [lookAndSay x | x <- sq]
    -- Part 1
    print $ "Length of #40: " ++ show (length $ sq !! 40)
    -- Part 2
    print $ "Length of #50: " ++ show (length $ sq !! 50)

