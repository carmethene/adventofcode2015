import Control.Applicative
import Data.List

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1..isqrt(n)], let (q, r) = divMod n x, r == 0]

presents :: Int -> Int
presents = (10 *) . sum . factors

main = do
    target <- read <$> readFile "input.txt"
    -- Part 1
    print $ findIndex (>= target) (map presents [0..])
