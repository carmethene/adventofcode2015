import Data.Char
import Data.Word
import Data.Bits
import Data.Maybe
import Data.Function.Memoize
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

readInstruction :: String -> Circuit -> Circuit
readInstruction str = Map.insert name wire where
    ws   = words str
    name = last ws
    wire = readWire $ takeWhile (/= "->") ws
    readWire :: [String] -> Instruction
    readWire s = i where
        i = case s of
            [x, "AND",    y] -> And (readInput x) (readInput y)
            [x, "OR",     y] -> Or (readInput x) (readInput y)
            [   "NOT",    x] -> Not (readInput x)
            [x, "LSHIFT", y] -> LeftShift (readInput x) (readInput y)
            [x, "RSHIFT", y] -> RightShift (readInput x) (readInput y)
            [x]              -> Only (readInput x)
        readInput :: String -> Input
        readInput i
            | all isDigit i = Set (read i)
            | otherwise     = Connect i

evalCircuit :: Circuit -> Wire -> Signal
evalCircuit c = evalWire where
    evalWire = memoize evalWire'
    evalWire' :: Wire -> Signal
    evalWire' w = evalInstruction (fromJust $ Map.lookup w c) where
        evalInstruction :: Instruction -> Signal
        evalInstruction i = s where
            s = case i of
                (And x y)        -> getInput x .&. getInput y
                (Or x y)         -> getInput x .|. getInput y
                (Not x)          -> complement $ getInput x
                (LeftShift x y)  -> shift (getInput x) (fromIntegral $ getInput y)
                (RightShift x y) -> shift (getInput x) (negate $ fromIntegral $ getInput y)
                (Only x)         -> getInput x
            getInput (Connect x) = evalWire x
            getInput (Set x)     = x

main = do
    input <- getContents
    let instructions = lines input
    let circuit = foldr readInstruction Map.empty instructions
    print $ "Wire: " ++ show (evalCircuit circuit "a")

