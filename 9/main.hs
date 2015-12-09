import Data.List
import Data.Maybe
import qualified Data.Map as Map

type Place = String
type Distance = Integer
type Road = (Place, Distance)
type Graph = Map.Map Place [Road]
type Route = [Place]

loadRoad :: String -> Graph -> Graph
loadRoad string graph = addRoad x y d (addRoad y x d graph) where
    [x, "to", y, "=", ds] = words string
    d = read ds
    addRoad :: Place -> Place -> Distance -> Graph -> Graph
    addRoad place destination distance graph = Map.insert place roads graph where
        road = (destination, distance)
        roads = case Map.lookup place graph of
                  Just rs -> road : rs
                  Nothing -> [road]

flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (x:xs) = x ++ flatten xs

routes :: Graph -> [Route]
routes graph = filter (\x -> length x == Map.size graph) allRoutes where
    allRoutes :: [Route]
    allRoutes = flatten [walkPlace graph place [place] | place <- Map.keys graph] where
        walkPlace :: Graph -> Place -> Route -> [Route]
        walkPlace graph place route = routes where
            remainingGraph = Map.delete place graph
            roads = fromMaybe [] (Map.lookup place graph)
            nextPlaces = filter (\p -> isJust $ Map.lookup p remainingGraph) [p | (p, _) <- roads]
            routes = case nextPlaces of
                       [] -> [route]
                       _  -> flatten [walkPlace remainingGraph p (route ++ [p]) | p <- nextPlaces] where

main = do
    input <- readFile "input.txt"
    let graph = foldr loadRoad Map.empty (lines input)
    print $ routes graph

