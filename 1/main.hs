import Data.List

step :: Integer -> Char -> Integer
step cur ins
    | ins == '(' = cur + 1
    | ins == ')' = cur - 1

position :: String -> Integer
position = foldl step 0

floors :: String -> [Integer]
floors = scanl step 0

main = do
    instructions <- getLine
    print $ "Final floor: " ++ show (position instructions)
    print $ "Basement at: " ++ show (elemIndex (-1) $ floors instructions)

