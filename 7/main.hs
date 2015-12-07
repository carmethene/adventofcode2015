import Data.Char
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map

type Wire = String
type Signal = Word16

data Input = Connect Wire | Set Signal deriving (Show)

data Instruction =
    And Input Input |
    Or Input Input |
    Not Input |
    LeftShift Input Input |
    RightShift Input Input |
    Only Input
    deriving (Show)

type Circuit = Map.Map Wire Instruction

readInput :: String -> Input
readInput i
    | all isDigit i = Set (read i)
    | otherwise     = Connect i

readWire :: [String] -> Instruction
readWire [x, "AND",    y] = And (readInput x) (readInput y)
readWire [x, "OR",     y] = Or (readInput x) (readInput y)
readWire [   "NOT",    x] = Not (readInput x)
readWire [x, "LSHIFT", y] = LeftShift (readInput x) (readInput y)
readWire [x, "RSHIFT", y] = RightShift (readInput x) (readInput y)
readWire [x]              = Only (readInput x)

readInstruction :: String -> Circuit -> Circuit
readInstruction str = Map.insert name wire where
    ws   = words str
    name = last ws
    wire = readWire $ takeWhile (/= "->") ws

evalWire :: Circuit -> Wire -> Signal
evalWire c w = evalInstruction c (fromJust $ Map.lookup w c)

evalInstruction :: Circuit -> Instruction -> Signal
evalInstruction c i = case i of
                        (And x y)        -> getInput x .&. getInput y
                        (Or x y)         -> getInput x .|. getInput y
                        (Not x)          -> complement $ getInput x
                        (LeftShift x y)  -> shift (getInput x) (fromIntegral $ getInput y)
                        (RightShift x y) -> shift (getInput x) (negate $ fromIntegral $ getInput y)
                        (Only x)         -> getInput x
                    where
                        getInput (Connect x) = evalWire c x
                        getInput (Set x)     = x

main = do
    input <- getContents
    let instructions = lines input
    let circuit = foldr readInstruction Map.empty instructions
    print $ "Wire a: " ++ show (evalWire circuit "a")

