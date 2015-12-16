{-# LANGUAGE OverloadedStrings #-}

-- Imports
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.Map as Map
 
-- Types
data Ingredient = Ingredient { capacity   :: Integer
                             , durability :: Integer
                             , flavor     :: Integer
                             , texture    :: Integer
                             , calories   :: Integer
                             } deriving (Show)

type IngredientMap = Map.Map BS.ByteString Ingredient

-- Parser
parseProperty :: BS.ByteString -> A.Parser Integer
parseProperty name = do
    A.skipSpace
    A.string name
    A.skipSpace
    A.signed A.decimal

parseIngredient :: A.Parser (BS.ByteString, Ingredient)
parseIngredient = do
    A.skipSpace
    name       <- A.takeWhile (/= ':')
    A.char ':'
    capacity   <- parseProperty "capacity"
    A.char ','
    durability <- parseProperty "durability"
    A.char ','
    flavor     <- parseProperty "flavor"
    A.char ','
    texture    <- parseProperty "texture"
    A.char ','
    calories   <- parseProperty "calories"
    A.endOfLine
    return (name, Ingredient capacity durability flavor texture calories)

parseIngredients :: A.Parser IngredientMap
parseIngredients = do
    ingredients <- A.many' parseIngredient
    return $ Map.fromList ingredients

-- Solver
main = do
    input <- BS.readFile "input.txt"
    let ingredients = A.parseOnly parseIngredients input
    print ingredients

