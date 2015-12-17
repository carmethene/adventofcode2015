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
auntMatchesSample :: [Sample] -> Aunt -> Bool
auntMatchesSample [] _ = True
auntMatchesSample (x:xs) aunt@(_, as) = match && auntMatchesSample xs aunt where
    (tc, tv) = x
    match = case lookup tc as of
      Nothing -> True
      Just av -> av == tv

desiredAunt = "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

main = do
    input <- BS.readFile "input.txt"
    let (Right aunts)   = A.parseOnly parseAunts input
    let (Right desired) = A.parseOnly (parseSample `A.sepBy` A.char ',') desiredAunt
    let matchingAunts = filter (auntMatchesSample desired) aunts
    print $ "Matching aunt: " ++ show (fst (head matchingAunts))

