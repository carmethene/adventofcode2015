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

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys

main = do
    instructions <- getLine
    let startHouse = (0,0)
    -- part 1
    let presents = houses startHouse instructions
    print $ "Num houses (1): " ++ show (length $ nub presents)
    -- part 2
    let splitPresents = houses startHouse ins1 ++ houses startHouse ins2 where (ins1, ins2) = split instructions
    print $ "Num houses (2): " ++ show (length $ nub splitPresents)

