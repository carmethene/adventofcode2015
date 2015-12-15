{-# LANGUAGE OverloadedStrings #-}

-- Imports
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as A

-- Types
type Name  = BS.ByteString
type Speed = Integer
type Time  = Integer
type Move  = (Speed, Time)
type Reindeer    = (Name, [Move])
type ReindeerMap = Map.Map Name [Move]

-- Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
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

main = do
    input <- BS.readFile "input.txt"
    let r = A.parseOnly parseReindeerMap input
    print r

