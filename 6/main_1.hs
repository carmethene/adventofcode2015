import Data.List
import Data.List.Split

data Operation = TurnOn | TurnOff | Toggle deriving (Show, Eq)
type Light = (Integer, Integer)
type Instruction = (Operation, Light, Light)
data State = LightOn | LightOff deriving (Show, Eq)

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

operate :: Light -> State -> Instruction -> State
operate (x, y) s (op, (xMin, yMin), (xMax, yMax))
    | x < xMin || x > xMax || y < yMin || y > yMax = s    -- out of range, no change
    | op == TurnOn  = LightOn
    | op == TurnOff = LightOff
    | op == Toggle && s == LightOn  = LightOff
    | op == Toggle && s == LightOff = LightOn

runInstructions :: [Instruction] -> Light -> State
runInstructions instructions light = foldl (operate light) LightOff instructions

main = do
    input <- getContents
    let instructions = map stringToInstruction $ lines input
    let lights = [(x,y) :: Light | x <- [0..999], y <- [0..999]]
    let states = map (runInstructions instructions) lights
    print $ "Num lights on: " ++ show (length $ filter (== LightOn) states)

