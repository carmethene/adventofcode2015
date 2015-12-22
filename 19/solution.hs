{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T
import Control.Applicative
import Data.List

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
replacements :: [Replacement] -> Molecule -> [Molecule]
replacements rs = replaceElem [] where
    replaceElem :: [Element] -> [Element] ->[Molecule]
    replaceElem xs []     = []
    replaceElem xs (y:ys) = replaceEach xs y ys ++ replaceElem (xs ++ [y]) ys
    replaceEach :: [Element] -> Element -> [Element] -> [Molecule]
    replaceEach h e t     = [h ++ m ++ t | (r, m) <- rs, r == e]

main = do
    input <- T.readFile "input.txt"
    let (Right calibration) = A.parseOnly parseCalibration input
    let (rs, m) = calibration
    let uniqueReplacements = nub $ replacements rs m
    print $ length uniqueReplacements

    -- let rs = [("x", ["a","b","c"]), ("y",["d","e","f"])]
    -- let m = ["w","x","y","z"]
    -- let r = replacements rs m
    -- print r
