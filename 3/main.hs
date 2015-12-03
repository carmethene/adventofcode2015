import Data.List

type House = (Integer, Integer)
type Instruction = Char

move :: House -> Instruction -> House
move (x,y) ins
    | ins == '<' = (x-1,y)
    | ins == '>' = (x+1,y)
    | ins == '^' = (x,y-1)
    | ins == 'v' = (x,y+1)

houses :: House -> [Instruction] -> [House]
houses = scanl move 

main = do
    instructions <- getLine
    let presents = houses (0,0) instructions
    print $ "Num houses: " ++ show (length $ nub presents)

