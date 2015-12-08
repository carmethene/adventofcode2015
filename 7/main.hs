import Data.Char
import Data.Word
import Data.Bits
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

loadWire :: Circuit -> String -> Circuit
loadWire c str = Map.insert wire ins c where
    ws   = words str
    wire = last ws
    ins  = readInstruction $ takeWhile (/= "->") ws where
        readInstruction :: [String] -> Instruction
        readInstruction s = i where
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

readWire :: Circuit -> Wire -> Signal
readWire c = evalWire where
    evalWire = memoize evalWire'
    evalWire' :: Wire -> Signal
    evalWire' w = evalInstruction (Map.lookup w c) where
        evalInstruction :: Maybe Instruction -> Signal
        evalInstruction i = s where
            s = case i of
                Just (And x y)        -> getInput x .&. getInput y
                Just (Or x y)         -> getInput x .|. getInput y
                Just (Not x)          -> complement $ getInput x
                Just (LeftShift x y)  -> shiftL (getInput x) (fromIntegral $ getInput y)
                Just (RightShift x y) -> shiftR (getInput x) (fromIntegral $ getInput y)
                Just (Only x)         -> getInput x
                Nothing               -> error $ "Invalid wire: " ++ w
            getInput (Connect x) = evalWire x
            getInput (Set x)     = x

main = do
    input <- getContents
    let instructions = lines input
    let circuit = foldl loadWire Map.empty instructions
    -- Part 1
    let signal = readWire circuit "a"
    print $ "Wire a (1): " ++ show signal
    -- Part 2
    let circuit2 = Map.insert "b" (Only $ Set signal) circuit
    let signal2 = readWire circuit2 "a"
    print $ "Wire a (2): " ++ show signal2

