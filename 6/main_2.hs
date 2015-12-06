import Data.List
import Data.List.Split

data Operation = TurnOn | TurnOff | Toggle deriving (Show, Eq)
type Light = (Integer, Integer)
type Instruction = (Operation, Light, Light)
type State = Integer

stringToOperation :: String -> Operation
stringToOperation str
    | str == "on"     = TurnOn
    | str == "off"    = TurnOff
    | str == "toggle" = Toggle

stringToLight :: String -> Light
stringToLight str = (x,y) where
    vals = splitOn "," str
    x    = read $ head vals
    y    = read $ vals !! 1

stringToInstruction :: String -> Instruction
stringToInstruction str
    | "turn " `isPrefixOf` str = stringToInstruction $ drop 5 str -- dropping "turn" makes parsing much simpler
    | otherwise              = (op, start, end) where
        ws    = words str
        op    = stringToOperation $ head ws
        start = stringToLight $ ws !! 1
        end   = stringToLight $ ws !! 3

operate :: Light -> State -> Instruction -> State
operate (x, y) s (op, (xMin, yMin), (xMax, yMax))
    | x < xMin || x > xMax || y < yMin || y > yMax = s    -- out of Range, no change
    | op == TurnOn  = s + 1
    | op == TurnOff = if s > 0 then s - 1 else 0
    | op == Toggle  = s + 2

runInstructions :: [Instruction] -> Light -> State
runInstructions instructions light = foldl (operate light) 0 instructions

main = do
    input <- getContents
    let instructions = map stringToInstruction $ lines input
    let lights = [(x,y) :: Light | x <- [0..999], y <- [0..999]]
    let states = map (runInstructions instructions) lights
    print $ "Total brightness: " ++ show (sum states)

