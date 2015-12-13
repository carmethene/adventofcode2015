import Data.Char
import Data.List

isNumberGroup :: Char -> Char -> Bool
isNumberGroup x y = (isNegative && isNumY) || (isNumX && isNumY) where
    isNegative = x == '-'
    isNumX = isNumber x
    isNumY = isNumber y

isNumberString :: String -> Bool
isNumberString (x:xs) = (isNegative && isNumberString xs) || (isNumX && isNumberString xs) where
    isNegative = x == '-'
    isNumX = isNumber x
isNumberString [] = True

main = do
    input <- getContents
    let groups = groupBy isNumberGroup input
    let numberStrings = filter isNumberString groups
    let numbers = map read numberStrings
    print $ "Sum: " ++ show (sum numbers) 

