{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import Control.Applicative
import Data.List
import Data.Maybe

-- Types
type Element     = String
type Molecule    = [Element]
type Replacement = (Element, Molecule)

data Input = Input { getReplacements :: [Replacement]
                   , getElectrons    :: [Molecule]
                   , getTarget       :: Molecule
                   } deriving (Show)

-- Parser
parseElement :: A.Parser Element
parseElement = let
    pClass :: String -> A.Parser String
    pClass cls = (: []) <$> A.satisfy (A.inClass cls) in
        (++) <$> pClass "A-Z" <*> A.option "" (pClass "a-z")

parseMolecule :: A.Parser Molecule
parseMolecule = A.many1 parseElement

parseReplacement :: A.Parser Replacement
parseReplacement = do
    src <- parseElement
    A.skipSpace
    A.string "=>"
    A.skipSpace
    tgt <- parseMolecule
    return (src, tgt)

parseElectron :: A.Parser Molecule
parseElectron = do
    A.char 'e'
    A.skipSpace
    A.string "=>"
    A.skipSpace
    parseMolecule

parseInput :: A.Parser Input
parseInput = do
    replacements <- parseReplacement `A.sepBy` A.endOfLine
    A.skipSpace
    electrons <- parseElectron `A.sepBy` A.endOfLine
    A.skipSpace
    target <- parseMolecule 
    return $ Input replacements electrons target

-- Solver
nextMolecules :: [Replacement] -> Molecule -> [Molecule]
nextMolecules rs = eachElement [] where
    eachElement :: [Element] -> [Element] -> [Molecule]
    eachElement xs []     = []
    eachElement xs (y:ys) = replaceElem xs y ys ++ eachElement (xs ++ [y]) ys
    replaceElem :: [Element] -> Element -> [Element] -> [Molecule]
    replaceElem h e t = [h ++ m ++ t | (r, m) <- rs, r == e]

stepsToMolecule :: [Replacement] -> [Molecule] -> Molecule -> Maybe Int
stepsToMolecule rs es tgt = let
    molFilter   = (<= length tgt) . length
    molecules   = es : [m | ms <- molecules, m <- map (nextMolecules rs) (filter molFilter ms)]
    steps       = zip [1..] molecules
    step        = find (\(i, ms) -> tgt `elem` ms) steps
    in case step of
        Just (i, _) -> Just i
        Nothing     -> Nothing

main = do
    input <- T.readFile "input.txt"
    let Right (Input rs es tgt) = A.parseOnly parseInput input
    -- Part 1
    let uniqueReplacements = nub $ nextMolecules rs tgt
    print $ "Num molecules: " ++ show (length uniqueReplacements)
    -- Part 2
    print $ "Num steps:     " ++ show (fromJust (stepsToMolecule rs es tgt))
