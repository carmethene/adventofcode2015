import Data.Matrix -- Types
type State = Matrix Bool

-- Parser
loadLights :: String -> State
loadLights str = matrix r c s where
    l = lines (filter (/= '\r') str)
    r = length l
    c = minimum (map length l)
    s (i, j) = let c = l !! (i - 1) !! (j - 1) in c == '#'

-- Solver
getNeighbours :: Int -> Int -> State -> [Bool]
getNeighbours i j m = map (\(i, j) -> getElem i j m) neighbours where
    inBounds i' j' = i' >= 1 && i' <= nrows m && j' >= 1 && j' <= ncols m
    notSelf  i' j' = i' /= i || j' /= j
    neighbours     = [ (i', j') | i' <- [i-1, i, i+1], j' <- [j-1, j, j+1],
                                  inBounds i' j' && notSelf i' j']

animate :: State -> State
animate m0 = matrix r c s where
    r = nrows m0
    c = ncols m0
    s (i, j) = let
        s0 = getElem i j m0 
        n  = length (filter (== True) (getNeighbours i j m0)) in
        if s0
           then n == 2 || n == 3
           else n == 3

fixCorners :: State -> State
fixCorners m0 = matrix r c s where
    r = nrows m0
    c = ncols m0
    s (i, j) = let
        cr = (i == 1 || i == r) && (j == 1 || j == c) in
        cr || getElem i j m0
 
main = do
    input <- getContents
    let initialState = loadLights input
    -- Part 1
    let states1 = iterate animate initialState
    print $ "Num lights (1): " ++ show (length (filter id (toList $ states1 !! 100)))
    -- Part 2
    let states2 = iterate (fixCorners . animate) (fixCorners initialState)
    print $ "Num lights (2): " ++ show (length (filter id (toList $ states2 !! 100)))

