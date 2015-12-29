import Data.List
import Data.Maybe

-- Types
data ItemAttrs = ItemAttrs { itemCost   :: Int
                           , itemDamage :: Int
                           , itemArmor  :: Int
                           } deriving (Show)

type Item = (String, ItemAttrs)

data Character = Character { charHitPoints :: Int
                           , charDamage    :: Int
                           , charArmor     :: Int
                           } deriving (Show)

-- Game logic
loadCharacter :: String -> Character
loadCharacter str = Character h d a where
    ws = words str
    h  = read $ ws !! 2
    d  = read $ ws !! 4
    a  = read $ ws !! 6

kills :: Character -> Character -> Bool
x `kills` y = deathIdx `mod` 2 == 0 where
    deathIdx = fromJust $ findIndex (\ (Character h _ _) -> h <= 0) (battle x y)
    battle :: Character -> Character -> [Character]
    battle x y = y' : x' : battle x' y' where
        y' = x `attack` y
        x' = y `attack` x
        attack :: Character -> Character -> Character
        (Character _ xd _)  `attack` (Character yh yd ya) = Character yh' yd ya where
            xd' = max (xd - ya) 1
            yh' = yh - xd'

-- The Shop
weapons :: [Item]
weapons = [ ("Dagger",     ItemAttrs  8 4 0)
          , ("Shortsword", ItemAttrs 10 5 0)
          , ("Warhammer",  ItemAttrs 25 6 0)
          , ("Longsword",  ItemAttrs 40 7 0)
          , ("Greataxe",   ItemAttrs 74 8 0)
          ]

armor :: [Item]
armor = [ ("Leather",    ItemAttrs  13 0 1)
        , ("Chainmail",  ItemAttrs  31 0 2)
        , ("Splintmail", ItemAttrs  53 0 3)
        , ("Bandedmail", ItemAttrs  75 0 4)
        , ("Platemail",  ItemAttrs 102 0 5)
        ]

rings :: [Item]
rings = [ ("Damage +1",  ItemAttrs  25 1 0)
        , ("Damage +2",  ItemAttrs  50 2 0)
        , ("Damage +2",  ItemAttrs 100 3 0)
        , ("Defense +2", ItemAttrs  20 0 1)
        , ("Defense +2", ItemAttrs  40 0 2)
        , ("Defense +2", ItemAttrs  80 0 3)
        ]

-- Starting player
player = Character 100 0 0

main = do
    input <- readFile "input.txt"
    let boss = loadCharacter input
    print $ player `kills` boss

