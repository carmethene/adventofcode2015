import Data.List
import qualified Data.Hash.MD5 as MD5

keys :: String -> [String]
keys salt = [salt ++ show num | num <- [0,1..]]

hashes :: [String] -> [String]
hashes = map (MD5.md5s . MD5.Str)

startsWithFiveZeroes :: String -> Bool
startsWithFiveZeroes ('0':'0':'0':'0':'0':_) = True
startsWithFiveZeroes _ = False

startsWithSixZeroes :: String -> Bool
startsWithSixZeroes ('0':'0':'0':'0':'0':'0':_) = True
startsWithSixZeroes _ = False

main = do
    salt <- getLine
    print $ "Five zeroes: " ++ show (findIndex startsWithFiveZeroes (hashes $ keys salt))
    print $ "Six zeroes:  " ++ show (findIndex startsWithSixZeroes  (hashes $ keys salt))

