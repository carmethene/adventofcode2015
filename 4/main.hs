import Data.List
import qualified Data.Hash.MD5 as MD5

keys :: String -> [String]
keys salt = [salt ++ show num | num <- [0,1..]]

hashes :: [String] -> [String]
hashes = map (MD5.md5s . MD5.Str)

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros ('0':'0':'0':'0':'0':_) = True
startsWithFiveZeros _ = False

startsWithSixZeros :: String -> Bool
startsWithSixZeros ('0':'0':'0':'0':'0':'0':_) = True
startsWithSixZeros _ = False

main = do
    salt <- getLine
    print $ "Five zeroes: " ++ show (findIndex startsWithFiveZeros (hashes $ keys salt))
    print $ "Six zeroes:  " ++ show (findIndex startsWithSixZeros  (hashes $ keys salt))

