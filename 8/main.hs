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

encodeString :: String -> String
encodeString str = "\"" ++ encodeChars str ++ "\"" where
    encodeChars :: String -> String
    encodeChars [] = []
    encodeChars ('\\':xs) = "\\\\" ++ encodeChars xs
    encodeChars ('\"':xs) = "\\\"" ++ encodeChars xs
    encodeChars (x:xs) = x : encodeChars xs

main = do
    input <- getContents
    let sentences = lines input
    -- Part 1
    let c = sum $ map numCodeChars sentences
    let m = sum $ map numMemoryChars sentences
    print $ "Answer 1: " ++ show (c - m)
    -- Part 2
    let e = sum $ map (numCodeChars . encodeString) sentences
    print $ "Answer 2: " ++ show (e - c)

