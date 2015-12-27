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
                   , getTarget       :: Molecule
                   } deriving (Show)

electron :: Element
electron = "e"

-- Parser
parseElement :: A.Parser Element
parseElement = let
    pClass :: String -> A.Parser String
    pClass cls = (: []) <$> A.satisfy (A.inClass cls) in
        (++) <$> pClass "A-Z" <*> A.option "" (pClass "a-z")

parseElectron :: A.Parser Element
parseElectron = do
    A.char 'e'
    return electron 

parseMolecule :: A.Parser Molecule
parseMolecule = A.many1 parseElement

parseReplacement :: A.Parser Replacement
parseReplacement = do
    src <- parseElement <|> parseElectron
    A.skipSpace
    A.string "=>"
    A.skipSpace
    tgt <- parseMolecule
    return (src, tgt)

parseInput :: A.Parser Input
parseInput = do
    replacements <- parseReplacement `A.sepBy` A.endOfLine
    A.skipSpace
    target <- parseMolecule 
    return $ Input replacements target

-- Solver
expandMolecule :: [Replacement] -> Molecule -> [Molecule]
expandMolecule rs = eachElement [] where
    eachElement :: [Element] -> [Element] -> [Molecule]
    eachElement xs []     = []
    eachElement xs (y:ys) = replaceElem xs y ys ++ eachElement (xs ++ [y]) ys
    replaceElem :: [Element] -> Element -> [Element] -> [Molecule]
    replaceElem h e t = [h ++ m ++ t | (r, m) <- rs, r == e]

contractMolecule :: [Replacement] -> Molecule -> [Molecule]
contractMolecule rs m = sort nextMolecules where
    nextMolecules :: [Molecule]
    nextMolecules = filter (/= []) $ concatMap (eachElement [] m) rs
    eachElement :: [Element] -> [Element] -> Replacement -> [Molecule]
    eachElement _  []     _ = []
    eachElement xs (y:ys) r = contract xs (y:ys) r : eachElement (xs ++ [y]) ys r
    contract :: Molecule -> Molecule -> Replacement -> Molecule
    contract xs []     (e, m:ms) = []
    contract xs ys     (e, [])   = xs ++ [e] ++ ys
    contract xs (y:ys) (e, m:ms) = if y == m
        then contract xs ys (e, ms)
        else []

solveMolecule :: [Replacement] -> Molecule -> [Molecule]
solveMolecule rs cur
    | cur == [electron] = [[electron]]
    | otherwise         = solveMolecules [cur] nextMolecules where
        nextMolecules = contractMolecule rs cur
        solveMolecules :: [Molecule] -> [Molecule] -> [Molecule]
        solveMolecules lst [] = []
        solveMolecules lst (x:xs) = let
            solveX = solveMolecule rs x in
            if solveX /= []
                then lst ++ solveX
                else solveMolecules lst xs

debug = do
    input <- T.readFile "input.txt"
    let Right (Input rs tgt) = A.parseOnly parseInput input
    let dbgMol = ["H", "P", "B", "F"]
    print $ solveMolecule rs dbgMol

main = do
    input <- T.readFile "input.txt"
    let Right (Input rs tgt) = A.parseOnly parseInput input
    -- Part 1
    let uniqueReplacements = nub $ expandMolecule rs tgt
    print $ "Num molecules: " ++ show (length uniqueReplacements)
    -- Part 2
    let stepsToElectron = solveMolecule rs tgt
    print $ "Num steps:     " ++ show ((length stepsToElectron) - 1)
