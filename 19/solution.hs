{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import Control.Applicative

-- Types
type Element     = String
type Molecule    = [Element]
type Replacement = (Element, Molecule)
type Calibration = ([Replacement], Molecule)

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

parseEmpty :: A.Parser ()
parseEmpty = do
    A.char 'e'
    A.skipSpace
    A.string "=>"
    A.skipSpace
    parseMolecule
    return ()

parseCalibration :: A.Parser Calibration
parseCalibration = do
    replacements <- parseReplacement `A.sepBy` A.endOfLine
    A.skipSpace
    parseEmpty `A.sepBy` A.endOfLine
    A.skipSpace
    initial <- parseMolecule 
    return (replacements, initial)

-- Solver
main = do
    input <- T.readFile "input.txt"
    let (Right calibration) = A.parseOnly parseCalibration input
    print calibration
