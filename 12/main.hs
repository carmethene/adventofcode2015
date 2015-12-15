{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative

-- JSON types
type JsonString = BS.ByteString
type JsonNumber = Integer
type JsonObject = Map.Map JsonString JsonValue
type JsonArray  = [JsonValue]
type JsonBool   = Bool
data JsonValue  = JVString JsonString
                | JVNumber JsonNumber
                | JVObject JsonObject
                | JVArray  JsonArray
                | JVBool   JsonBool
                | JVNull
                deriving (Show, Eq)

data JsonDocument = JDObject JsonObject
                  | JDArray JsonArray
                  deriving Show

-- JSON parser
parseJsonString :: A.Parser JsonString
parseJsonString = do
    A.char '\"'
    str <- A.takeWhile (/= '\"')
    A.char '\"'
    return str

parseJsonNumber :: A.Parser JsonNumber
parseJsonNumber = A.signed A.decimal

parseJsonObject :: A.Parser JsonObject
parseJsonObject = do
    A.char '{'
    keyVals <- parseKeyVal `A.sepBy` A.char ','
    A.char '}'
    return $ Map.fromList keyVals where
        parseKeyVal :: A.Parser (JsonString, JsonValue)
        parseKeyVal = do
            key <- parseJsonString
            A.char ':'
            A.skipSpace
            value <- parseJsonValue
            return (key, value)

parseJsonArray :: A.Parser JsonArray
parseJsonArray = do
    A.char '['
    vals <- parseJsonValue `A.sepBy` A.char ','
    A.char ']'
    return vals
 
parseJsonBool :: A.Parser JsonBool
parseJsonBool = true <|> false where
    true  = A.string "true"  >> return True
    false = A.string "false" >> return False

parseJsonValue :: A.Parser JsonValue
parseJsonValue = A.choice [parseJVString, parseJVNumber, parseJVObject, parseJVArray, parseJVBool, parseJVNull] where
    parseJVString = JVString <$> parseJsonString
    parseJVNumber = JVNumber <$> parseJsonNumber
    parseJVObject = JVObject <$> parseJsonObject
    parseJVArray  = JVArray  <$> parseJsonArray
    parseJVBool   = JVBool   <$> parseJsonBool
    parseJVNull   = A.string "null" >> return JVNull

parseJsonDocument :: A.Parser JsonDocument
parseJsonDocument = object <|> array where
    object = JDObject <$> parseJsonObject
    array  = JDArray  <$> parseJsonArray

-- JSON walker
sumJsonObject :: JsonObject -> JsonNumber
-- sumJsonObject obj = sum (map sumJsonValue (Map.elems obj))
sumJsonObject obj = if (elem (JVString "red") vals)
                       then 0
                       else sum (map sumJsonValue vals) where
                           vals = Map.elems obj

sumJsonArray :: JsonArray -> JsonNumber
sumJsonArray = foldr ((+) . sumJsonValue) 0

sumJsonValue :: JsonValue -> JsonNumber
sumJsonValue val = case val of
    JVObject obj -> sumJsonObject obj
    JVArray  arr -> sumJsonArray arr
    JVNumber num -> num
    _            -> 0

sumJsonDocument :: JsonDocument -> JsonNumber
sumJsonDocument doc = case doc of
    JDObject obj -> sumJsonObject obj
    JDArray  arr -> sumJsonArray arr

-- Entry
main = do
    input <- BS.readFile "input.txt"
    let (Right doc) = A.parseOnly parseJsonDocument input
    print $ "Sum: " ++ show (sumJsonDocument doc)

