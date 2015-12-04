import Data.List
import qualified Data.Hash.MD5 as MD5

keys :: String -> [String]
keys salt = [salt ++ show num | num <- [0,1..]]

hashes :: [String] -> [String]
hashes = map (MD5.md5s . MD5.Str)

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros ('0':'0':'0':'0':'0':_) = True
startsWithFiveZeros _ = False

main = do
    salt <- getLine
    return $ findIndex startsWithFiveZeros (hashes $ keys salt)

