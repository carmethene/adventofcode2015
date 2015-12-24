import Control.Applicative
import Data.List

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1..isqrt(n)], let (q, r) = divMod n x, r == 0]

presents1 :: Int -> Int
presents1 = (10 *) . sum . factors

presents2 :: Int -> Int
presents2 n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n

main = do
    target <- read <$> readFile "input.txt"
    -- Part 1
    print $ "House 1: " ++ show (findIndex (>= target) (map presents1 [0..]))
    -- Part 2
    print $ "House 2: " ++ show (findIndex (>= target) (map presents2 [0..]))
