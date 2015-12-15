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

-- High order functions
foldrJsonDocument :: (JsonValue -> a -> a) -> a -> JsonDocument -> a
foldrJsonDocument f x d = let
        foldrJsonObject :: (JsonValue -> a -> a) -> a -> JsonObject -> a
        foldrJsonObject f x o = foldr f x (Map.elems o)
        foldrJsonArray :: (JsonValue -> a -> a) -> a -> JsonArray -> a
        foldrJsonArray = foldr
        reduceJsonValue :: (JsonValue -> a -> a) -> JsonValue -> a -> a
        reduceJsonValue f val acc =
            case val of
              JVObject obj -> foldrJsonObject (reduceJsonValue f) acc obj
              JVArray  arr -> foldrJsonArray (reduceJsonValue f) acc arr
              otherwise    -> f val acc
        in case d of
             JDObject obj -> foldrJsonObject (reduceJsonValue f) x obj
             JDArray  arr -> foldrJsonArray (reduceJsonValue f) x arr

filterJsonDocument :: (JsonValue -> Bool) -> JsonDocument -> JsonDocument
filterJsonDocument f d = let
        filterJsonObject :: (JsonValue -> Bool) -> JsonObject -> JsonObject
        filterJsonObject f o = recurseMap where
                userFilter = Map.filter f o
                recurseMap = Map.map (filterJsonValue f) userFilter
        filterJsonArray :: (JsonValue -> Bool) -> JsonArray -> JsonArray
        filterJsonArray f a = recurseMap where
                userFilter = filter f a
                recurseMap = map (filterJsonValue f) userFilter
        filterJsonValue :: (JsonValue -> Bool) -> JsonValue -> JsonValue
        filterJsonValue f val =
            case val of
              JVObject obj -> JVObject $ filterJsonObject f obj
              JVArray arr  -> JVArray $ filterJsonArray f arr
              _            -> val
        in case d of 
             JDObject obj -> JDObject (filterJsonObject f obj)
             JDArray  arr -> JDArray (filterJsonArray f arr)

-- Low order functions
sumValues :: JsonValue -> JsonNumber -> JsonNumber
sumValues val acc = case val of
                      JVNumber num -> num + acc
                      _            -> acc

isRedObject :: JsonValue -> Bool
isRedObject val = case val of
                    JVObject obj -> JVString "red" `notElem` Map.elems obj
                    _            -> True

-- Entry
main = do
    input <- BS.readFile "input.txt"
    let (Right doc) = A.parseOnly parseJsonDocument input
    -- Part 1:
    print $ "Sum (1): " ++ show (foldrJsonDocument sumValues 0 doc)
    -- Part 2:
    let filteredDoc = filterJsonDocument isRedObject doc
    print $ "Sum (2): " ++ show (foldrJsonDocument sumValues 0 filteredDoc)

