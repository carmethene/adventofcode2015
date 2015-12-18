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
    value    <- A.decimal
    return (compound, value)

parseAunt :: A.Parser Aunt
parseAunt = do
    A.string "Sue "
    num     <- A.decimal
    A.char ':'
    samples <- parseSample `A.sepBy` A.char ','
    A.endOfLine
    return (num, samples)

-- Solver

-- Part 1: Exact match
auntMatchesSample1 :: [Sample] -> Aunt -> Bool
auntMatchesSample1 [] _ = True
auntMatchesSample1 (x:xs) aunt@(_, as) = match && auntMatchesSample1 xs aunt where
    (tc, tv) = x
    match    = case lookup tc as of
      Nothing -> True
      Just av -> av == tv

-- Part 2: Range match
auntMatchesSample2 :: [Sample] -> Aunt -> Bool
auntMatchesSample2 [] _ = True
auntMatchesSample2 (x:xs) aunt@(_, as) = match && auntMatchesSample2 xs aunt where
    (tc, tv) = x
    match    = case lookup tc as of
      Nothing -> True
      Just av -> case tc of
                   "cats"        -> av >  tv
                   "trees"       -> av >  tv
                   "pomeranians" -> av <  tv
                   "goldfish"    -> av <  tv
                   otherwise     -> av == tv

-- Target
desiredAunt = "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

main = do
    input <- BS.getContents
    let (Right aunts)   = A.parseOnly (A.many' parseAunt) input
    let (Right desired) = A.parseOnly (parseSample `A.sepBy` A.char ',') desiredAunt
    -- Part 1
    let matchingAunts1 = filter (auntMatchesSample1 desired) aunts
    print $ "Matching aunt (1): " ++ show (fst (head matchingAunts1))
    -- Part 2
    let matchingAunts2 = filter (auntMatchesSample2 desired) aunts
    print $ "Matching aunt (2): " ++ show (fst (head matchingAunts2))

