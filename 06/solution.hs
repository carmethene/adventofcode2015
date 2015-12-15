import Data.List
import Data.List.Split

-- Types
data Operation    = TurnOn | TurnOff | Toggle deriving (Show, Eq)
type Light        = (Integer, Integer)
type Instruction  = (Operation, Light, Light)
data StateBool    = LightOn | LightOff deriving (Show, Eq)
type StateInteger = Integer

-- Parser
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
    | otherwise                = (op, start, end) where
        ws    = words str
        op    = stringToOperation $ head ws
        start = stringToLight $ ws !! 1
        end   = stringToLight $ ws !! 3

-- Solver for Part 1
operateBool :: Light -> StateBool -> Instruction -> StateBool
operateBool (x, y) s (op, (xMin, yMin), (xMax, yMax))
    | x < xMin || x > xMax || y < yMin || y > yMax = s    -- out of range, no change
    | op == TurnOn  = LightOn
    | op == TurnOff = LightOff
    | op == Toggle && s == LightOn  = LightOff
    | op == Toggle && s == LightOff = LightOn

runInstructionsBool :: [Instruction] -> Light -> StateBool
runInstructionsBool instructions light = foldl (operateBool light) LightOff instructions

-- Solver for Part 2
operateInteger :: Light -> StateInteger -> Instruction -> StateInteger
operateInteger (x, y) s (op, (xMin, yMin), (xMax, yMax))
    | x < xMin || x > xMax || y < yMin || y > yMax = s    -- out of range, no change
    | op == TurnOn  = s + 1
    | op == TurnOff = if s > 0 then s - 1 else 0
    | op == Toggle  = s + 2

runInstructionsInteger :: [Instruction] -> Light -> StateInteger
runInstructionsInteger instructions light = foldl (operateInteger light) 0 instructions

main = do
    input <- getContents
    let instructions = map stringToInstruction $ lines input
    let lights = [(x,y) :: Light | x <- [0..999], y <- [0..999]]
    -- Part 1
    let statesBool = map (runInstructionsBool instructions) lights
    print $ "Num lights on (1): " ++ show (length $ filter (== LightOn) statesBool)
    -- Part 2
    let statesInteger = map (runInstructionsInteger instructions) lights
    print $ "Num lights on (2): " ++ show (sum statesInteger)

