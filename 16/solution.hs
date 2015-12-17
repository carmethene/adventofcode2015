{-# LANGUAGE OverloadedStrings #-}

-- Imports
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS

-- Types
type Compound = BS.ByteString
type Sample   = (Compound, Integer)
type Aunt     = (Integer, [Sample])

-- Parser
parseSample :: A.Parser Sample
parseSample = do
    A.skipSpace
    compound <- A.takeWhile (/= ':')
    A.char ':'
    A.skipSpace
    value <- A.decimal
    return (compound, value)

parseAunt :: A.Parser Aunt
parseAunt = do
    A.string "Sue "
    num <- A.decimal
    A.char ':'
    samples <- parseSample `A.sepBy` A.char ','
    A.endOfLine
    return (num, samples)

parseAunts :: A.Parser [Aunt]
parseAunts = A.many' parseAunt
 
-- Solver
main = do
    input <- BS.readFile "test.txt"
    let (Right aunts) = A.parseOnly parseAunts input
    print aunts

