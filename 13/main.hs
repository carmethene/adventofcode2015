{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative

type Name = BS.ByteString
type Happiness = Integer
type Relationship = (Name, Name, Happiness)
type RelationshipGraph = Map.Map Name (Map.Map Name Happiness)

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

main = do
    input <- BS.readFile "input.txt"
    let test = A.parseOnly parseRelationships input
    print test
