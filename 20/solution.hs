import Control.Applicative

elves :: Int -> [Int]
elves h = [e | e <- [1..h], h `mod` e == 0]

presents :: Int -> Int
presents = (10 *) . sum . elves

houses :: [(Int, Int)]
houses = zip h (map presents h) where h = [1..]

main = do
    target <- read <$> readFile "input.txt"
    -- Part 1
    let housesAbove = dropWhile (\(n, p) -> p < target) houses
    print $ head housesAbove
