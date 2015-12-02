import Data.List.Split
import Data.List

type Package = (Integer, Integer, Integer)

stringToPackage :: String -> Package
stringToPackage str = let [a,b,c] = splitOn "x" str
                          [x,y,z] = sort [read a, read b, read c]
                      in (x,y,z)

requiredPaper :: Package -> Integer
requiredPaper (x,y,z) = area + slack where
    area  = (2*x*y) + (2*y*z) + (2*z*x)
    slack = x*y

requiredRibbon :: Package -> Integer
requiredRibbon (x,y,z) = perimeter + bow where
    perimeter = (2*x) + (2*y)
    bow       = x*y*z

main = do
    input <- getContents
    let packages = map stringToPackage (Prelude.lines input)
    print $ "Paper:  " ++ show (sum $ map requiredPaper packages)
    print $ "Ribbon: " ++ show (sum $ map requiredRibbon packages)

