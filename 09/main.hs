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
        road  = (destination, distance)
        roads = case Map.lookup place graph of
                  Just rs -> road : rs
                  Nothing -> [road]

routes :: Graph -> [Route]
routes graph = filter (\x -> length x == Map.size graph) everyRoute where
    everyRoute :: [Route]
    everyRoute = concatMap (\x -> traverseRoutes graph x [x]) (Map.keys graph) where
        traverseRoutes :: Graph -> Place -> Route -> [Route]
        traverseRoutes graph place route = routes where
            remainingGraph = Map.delete place graph
            roads          = fromMaybe [] (Map.lookup place graph)
            nextPlaces     = filter (\p -> isJust $ Map.lookup p remainingGraph) [p | (p, _) <- roads]
            routes         = case nextPlaces of
                               [] -> [route]
                               _  -> concatMap (\x -> traverseRoutes remainingGraph x (route ++ [x])) nextPlaces

routeLength :: Graph -> Route -> Distance
routeLength graph route = sum [getDistance x y | (x, y) <- zip route (tail route)] where
    getDistance :: Place -> Place -> Distance
    getDistance x y = distance where
        roads         = fromJust $ Map.lookup x graph
        (_, distance) = fromJust $ find (\(r,_) -> r == y) roads

main = do
    input <- getContents 
    let graph = foldr loadRoad Map.empty (lines input)
    let routeLengths = map (routeLength graph) (routes graph)
    -- Part 1
    print $ "Minimum distance: " ++ show (minimum routeLengths)
    -- Part 2
    print $ "Maximum distance: " ++ show (maximum routeLengths)

