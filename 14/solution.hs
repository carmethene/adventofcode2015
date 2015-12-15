{-# LANGUAGE OverloadedStrings #-}

-- Imports
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as A

-- Types
type Name     = BS.ByteString
type Speed    = Integer
type Time     = Integer
type Distance = Integer
type Move     = (Speed, Time)
type Reindeer    = (Name, [Move])
type ReindeerMap = Map.Map Name [Move]

-- Parser
parseReindeer :: A.Parser Reindeer
parseReindeer = do
    name <- A.takeWhile (/= ' ')
    A.string " can fly "
    flySpeed <- A.decimal
    A.string " km/s for "
    flyTime <- A.decimal
    A.string " seconds, but then must rest for "
    restTime <- A.decimal
    A.string " seconds."
    A.endOfLine
    return (name, [(flySpeed, flyTime), (0, restTime)])

parseReindeerMap :: A.Parser ReindeerMap
parseReindeerMap = do
    reindeer <- A.many' parseReindeer
    return $ Map.fromList reindeer

-- Solver

-- A move: (StartTime, StartDistance, EndTime, EndDistance, SpeedOfMove)
type State = (Time, Distance, Time, Distance, Speed)

progressAtTime :: Time -> [Move] -> Distance
progressAtTime time moves = distance  where
    distance       = d0 + s * (time - t0)
    (t0, d0, _, _, s) = head $ dropWhile (\(_, _, t, _, _) -> t < time) (stateList moves)
    stateList :: [Move] -> [State]
    stateList = tail . scanl nextState (0, 0, 0, 0, 0) . cycle where
        nextState :: State -> Move -> State
        nextState (_, _, t0, d0, _) (s, t) = (t0, d0, t0 + t, d0 + s*t, s)

main = do
    input <- BS.readFile "input.txt"
    let (Right reindeer) = A.parseOnly parseReindeerMap input
    -- Part 1
    let progressMap = Map.map (progressAtTime 2503) reindeer
    print $ "Max distance: " ++ show (maximum $ Map.elems progressMap)
    -- Part 2
    let distanceMap = Map.map (\r -> map (`progressAtTime` r) [0..]) reindeer
    print $ "Max points:   " ++ show (0)

