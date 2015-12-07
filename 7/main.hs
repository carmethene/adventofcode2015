import Data.Char
import Data.Word
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map

type Wire = String
type Signal = Word16

data Instruction =
    And Wire Wire |
    Or Wire Wire |
    Not Wire |
    LeftShift Wire Signal |
    RightShift Wire Signal |
    Connect Wire |
    Set Signal
    deriving (Show)

type Circuit = Map.Map Wire Instruction

readWire :: [String] -> Instruction
readWire [x, "AND",    y] = And x y
readWire [x, "OR",     y] = Or x y
readWire [   "NOT",    x] = Not x
readWire [x, "LSHIFT", y] = LeftShift x (read y)
readWire [x, "RSHIFT", y] = RightShift x (read y)
readWire [x]
    | any isAlpha x = Connect x
    | otherwise     = Set (read x)

readInstruction :: String -> Circuit -> Circuit
readInstruction str = Map.insert name wire where
    ws   = words str
    name = last ws
    wire = readWire $ takeWhile (/= "->") ws

evalWire :: Circuit -> Wire -> Signal
evalWire c w = evalInstruction c (fromJust $ Map.lookup w c)

evalInstruction :: Circuit -> Instruction -> Signal
evalInstruction c (And x y)        = evalWire c x .&. evalWire c y
evalInstruction c (Or x y)         = evalWire c x .|. evalWire c y
evalInstruction c (Not x)          = complement $ evalWire c x
evalInstruction c (LeftShift x y)  = shift (evalWire c x) (fromIntegral y)
evalInstruction c (RightShift x y) = shift (evalWire c x) (negate $ fromIntegral y)
evalInstruction c (Connect x)      = evalWire c x
evalInstruction c (Set x)          = x

main = do
    input <- getContents
    let instructions = lines input
    let circuit = foldr readInstruction Map.empty instructions
    print $ "Wire a: " ++ show (evalWire circuit "a")

