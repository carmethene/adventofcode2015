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

main = do
    input <- readFile "test.txt"
    let initialState = loadLights input
    print initialState

