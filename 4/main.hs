import Data.List
import Data.String.Utils
import qualified Data.Hash.MD5 as MD5

keys :: String -> [String]
keys salt = [salt ++ show num | num <- [0,1..]]

hashes :: [String] -> [String]
hashes = map (MD5.md5s . MD5.Str)

main = do
    salt <- getLine
    print $ "Five zeroes: " ++ show (findIndex (startswith "00000") (hashes $ keys salt))
    print $ "Six zeroes:  " ++ show (findIndex (startswith "000000") (hashes $ keys salt))

