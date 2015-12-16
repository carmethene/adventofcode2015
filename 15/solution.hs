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

type Recipe = [Integer]

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

-- A list of all possible recipes
allRecipes :: Integer -> Integer -> [Recipe]
allRecipes 1 t = [[t]]
allRecipes n t = [ x:xs | x <- [0..t], xs <- allRecipes (n-1) (t-x)]

-- Reduce a recipe to a score
type Property = (Integer, Integer, Integer, Integer, Integer)

recipeScore :: [Ingredient] -> Recipe -> Integer
recipeScore x y = calculateScore $ clampProperty $ sumProperties $ properties x y

calculateScore :: Property -> Integer
calculateScore (v,w,x,y,z) = v*w*x*y

clampProperty :: Property -> Property
clampProperty (v,w,x,y,z) = (m v, m w, m x, m y, m z) where m = max 0

sumProperties :: [Property] -> Property
sumProperties = foldr1 sumProperty where
    sumProperty (v0,w0,x0,y0,z0) (v1,w1,x1,y1,z1) = (v0+v1,w0+w1,x0+x1,y0+y1,z0+z1)

properties :: [Ingredient] -> Recipe -> [Property]
properties = zipWith property where
    property i r = (cap, dur, flv, txt, cal) where
        cap = r * capacity i
        dur = r * durability i
        flv = r * flavor i
        txt = r * texture i
        cal = r * calories i

-- Solver
main = do
    input <- BS.readFile "input.txt"
    let (Right ingredientMap) = A.parseOnly parseIngredients input
    let ingredients = Map.elems ingredientMap
    let totalSize = 100 
    let recipes = allRecipes (fromIntegral $ length ingredients) totalSize
    let allScores = map (recipeScore ingredients) $ allRecipes (fromIntegral $ length ingredients) totalSize
    print $ "Max score: " ++ show (maximum allScores)

