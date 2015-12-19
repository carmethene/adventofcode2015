import Data.Matrix
-- Types
type State = Matrix Bool

-- Parser
loadLights :: String -> State
loadLights str = matrix r c f where
    s = lines str
    r  = length s
    c  = minimum (map length s)
    f (i, j) = let c = s !! (i - 1) !! (j - 1) in c == '#'

-- Solver
getNeighbours :: Int -> Int -> State -> [Bool]
getNeighbours i j m = map (\(i, j) -> getElem i j m) neighbours where
    inBounds i' j' = i' >= 1 && i' <= nrows m && j' >= 1 && j' <= ncols m
    notSelf  i' j' = i' /= i || j' /= j
    neighbours     = [ (i', j') | i' <- [i-1, i, i+1], j' <- [j-1, j, j+1],
                                  inBounds i' j' && notSelf i' j']

animate :: State -> State
animate m0 = matrix r c f where
    r = nrows m0
    c = ncols m0
    f (i, j) = let
        s0 = getElem i j m0 
        n  = length (filter (== True) (getNeighbours i j m0)) in
        if s0
           then n == 2 || n == 3
           else n == 3

main = do
    input <- getContents
    let initialState = loadLights input
    let states = iterate animate initialState
    print $ length (filter id (toList $ states !! 100))

