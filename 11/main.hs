import Data.List

incrementString :: String -> String
incrementString = reverse . incrementWithCarry True . reverse where
    incrementWithCarry :: Bool -> String -> String
    incrementWithCarry _ []     = []
    incrementWithCarry carry (x:xs) = if carry then incrementChar x : rest else x : rest where
        incrementChar :: Char -> Char
        incrementChar c = if c == 'z' then 'a' else succ c
        newCarry = carry && x == 'z'
        rest = incrementWithCarry newCarry xs

isCompliant :: String -> Bool
isCompliant str = increasingStraightThree str && noBadChars str && containsTwoPairs str where
    increasingStraightThree :: String -> Bool
    increasingStraightThree (x:y:z:xyzs) = (succ x == y && succ y == z) || increasingStraightThree (y:z:xyzs)
    increasingStraightThree _ = False
    noBadChars :: String -> Bool
    noBadChars str = null ("iol" `intersect` str)
    containsTwoPairs :: String -> Bool 
    containsTwoPairs str = length (filter (\x -> length x == 2) (group str)) >= 2

main = do
    input <- getLine
    let allPasswords = iterate incrementString input
    let validPasswords = filter isCompliant allPasswords
    print $ "Next password: " ++ head validPasswords

