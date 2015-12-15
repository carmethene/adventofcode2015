{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative
import Data.List
import Data.Maybe

-- Types
type Name = BS.ByteString
type Happiness = Integer
type Relationship = (Name, Name, Happiness)
type RelationshipGraph = Map.Map Name (Map.Map Name Happiness)
type Seating = [Name]

-- Parser
parseScale :: A.Parser Happiness
parseScale = gain <|> lose where
    gain = A.string "gain" >> return 1
    lose = A.string "lose" >> return (-1)

parseRelationship :: A.Parser Relationship
parseRelationship = do
    sourceName <- A.takeWhile (/= ' ')
    A.string " would "
    scale      <- parseScale
    A.char ' '
    happiness  <- A.decimal
    A.string " happiness units by sitting next to "
    targetName <- A.takeWhile (/= '.')
    A.char '.'
    A.endOfLine
    return (sourceName, targetName, scale * happiness)

parseRelationships :: A.Parser RelationshipGraph
parseRelationships = do
    relationships <- A.many' parseRelationship
    return $ foldr foldRelationships Map.empty relationships where
    foldRelationships :: Relationship -> RelationshipGraph -> RelationshipGraph
    foldRelationships (src, tgt, hpp) inGraph =
        Map.insert src tgtMap inGraph where
            tgtMap :: Map.Map Name Happiness
            tgtMap = case Map.lookup src inGraph of
                       Just g  -> Map.insert tgt hpp g
                       Nothing -> Map.insert tgt hpp Map.empty

-- Set of all possibilities
allSeatings :: RelationshipGraph -> [Seating]
allSeatings = permutations . Map.keys

-- Cost of each possibility
seatingHappiness :: RelationshipGraph -> Seating -> Happiness
seatingHappiness g s = totalCost where
    cost (src, tgt) = fromJust $ Map.lookup tgt tgtGraph where
        tgtGraph = fromJust $ Map.lookup src g
    pairs     = (last s, head s) : (head s, last s) : zip s (tail s) ++ zip (tail s) s
    costs     = map cost pairs
    totalCost = sum costs

main = do
    input <- BS.readFile "input.txt"
    let (Right relationships) = A.parseOnly parseRelationships input
    let values = map (seatingHappiness relationships) (allSeatings relationships)
    print $ "Max happiness: " ++ show (maximum values)

