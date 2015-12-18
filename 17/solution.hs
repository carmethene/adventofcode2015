main = do
    input <- readFile "input.txt"
    let containers = map (read :: String -> Int) $ lines input
    print containers

