numCodeChars :: String -> Int
numCodeChars = length

numMemoryChars :: String -> Int
numMemoryChars str = countWithEscapes (tail $ init str) where
    countWithEscapes :: String -> Int
    countWithEscapes [] = 0
    countWithEscapes ('\\':'\\':xs) = 1 + countWithEscapes xs
    countWithEscapes ('\\':'"':xs) = 1 + countWithEscapes xs
    countWithEscapes ('\\':'x':_:_:xs) = 1 + countWithEscapes xs
    countWithEscapes (x:xs) = 1 + countWithEscapes xs

main = do
    input <- readFile "input.txt"
    let sentences = lines input
    let c = sum $ map numCodeChars sentences
    let m = sum $ map numMemoryChars sentences
    print $ "Answer 1: " ++ show (c - m)

