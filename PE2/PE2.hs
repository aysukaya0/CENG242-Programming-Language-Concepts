{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PE2 where
import System.Console.Terminfo (restoreDefaultColors)

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

                                                           
-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp EmptyTree _ = (hp, 0)
traversePath hp dungeon path = traversee hp 0 dungeon path

--helper functions for traversePath
findFightEffect :: Integer -> Integer -> Enemy -> (Integer, Integer)
findFightEffect hp gold (Enemy name decreaseHp increaseGold) = let newHp = hp - decreaseHp
                                                                   newGold = gold + increaseGold
                                                               in (newHp, newGold)

findTreasureEffect :: Integer -> Integer -> Loot -> (Integer, Integer)
findTreasureEffect hp gold (Gold number) = let newGold = gold + number in (hp, newGold)
findTreasureEffect hp gold (Potion number) = let newHp = hp + number in (newHp, gold)

findEncountersEffect :: Integer -> Integer -> [Encounter] -> (Integer,Integer)
findEncountersEffect hp gold [] = (hp,gold)
findEncountersEffect hp gold ((Fight name):xs) = let (newHp, newGold) = findFightEffect hp gold name
                                                 in findEncountersEffect newHp newGold xs
findEncountersEffect hp gold ((Treasure name):xs) = let (newHp, newGold) = findTreasureEffect hp gold name
                                                    in findEncountersEffect newHp newGold xs

traversee :: Integer -> Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversee hp gold (Leaf name encounters) [] = findEncountersEffect hp gold encounters
traversee hp gold _ [] = (hp, gold)
traversee hp gold (Node name [] nodeList) (x:xs) = let newDestination = nodeList !! x in traversee hp gold newDestination xs 
traversee hp gold (Node name encounters nodeList) (x:xs) = let (newHp, newGold) = findEncountersEffect hp gold encounters
                                                               newDestination = nodeList !! x
                                                           in traversee newHp newGold newDestination xs



-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain _ EmptyTree = 0
findMaximumGain hp dungeon = let list = findMax hp 0 dungeon
                                 result = maximum list
                             in result

--helper functions of findMaximumGain
findMax :: Integer -> Integer -> Dungeon -> [Integer]
findMax hp gold (Leaf name encounters) = let (newHp, newGold) = calculateEncounter hp gold encounters
                                         in [newGold]
findMax hp gold (Node name encounters []) = [gold]
findMax hp gold (Node name encounters nodeList) = let (newHp, newGold) = calculateEncounter hp gold encounters 
                                                  in if newHp <= 0 then [newGold]
                                                     else let list = [findMax newHp newGold x | x<-nodeList] 
                                                              result = concat list
                                                          in result

enemyHpCalc :: Integer -> Enemy -> Integer
enemyHpCalc hp (Enemy name decreaseHp gold) = let newHp = hp - decreaseHp in newHp

calculateEncounter :: Integer -> Integer -> [Encounter] -> (Integer, Integer)
calculateEncounter hp gold [] = (hp,gold)
calculateEncounter hp gold ((Fight name):xs) = let newHp = enemyHpCalc hp name in if newHp <= 0 then (newHp, gold)
                                                                                  else let (newHp, newGold) = findFightEffect hp gold name
                                                                                       in calculateEncounter newHp newGold xs
calculateEncounter hp gold ((Treasure name):xs) = let (newHp, newGold) = findTreasureEffect hp gold name
                                                  in calculateEncounter newHp newGold xs


-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths 0 _ = EmptyTree
findViablePaths hp dungeon = let dungeonMap = findPath hp 0 dungeon
                                 result = nodeToLeaf $ dungeonWithoutEmptyTree dungeonMap
                             in result

--helper functions of findViablePaths
findPath :: Integer -> Integer -> Dungeon -> Dungeon
findPath hp gold (Leaf name encounters) = let (newHp, newGold) = calculateEncounter hp gold encounters
                                          in if newHp <= 0 then EmptyTree
                                             else Leaf name encounters
findPath hp gold (Node name encounters nodeList) = let (newHp, newGold) = calculateEncounter hp gold encounters
                                                   in if newHp <= 0 then EmptyTree
                                                      else Node name encounters [findPath newHp newGold x | x<-nodeList]
                                                       
removeEmptyTree :: [Tree a b] -> [Tree a b]
removeEmptyTree [] = []
removeEmptyTree (EmptyTree:rest) = removeEmptyTree rest
removeEmptyTree ((Node name encounters nodeList):rest) = Node name encounters (removeEmptyTree nodeList) : removeEmptyTree rest
removeEmptyTree ((Leaf name encounters):rest) = Leaf name encounters : removeEmptyTree rest

dungeonWithoutEmptyTree :: Dungeon -> Dungeon
dungeonWithoutEmptyTree (Leaf name encounters) = Leaf name encounters
dungeonWithoutEmptyTree (Node name encounters []) = Node name encounters []
dungeonWithoutEmptyTree (Node name encounters nodeList) = Node name encounters (removeEmptyTree nodeList)

nodeToLeaf :: Dungeon -> Dungeon
nodeToLeaf (Leaf name encounters) = Leaf name encounters
nodeToLeaf (Node name encounters []) = Leaf name encounters 
nodeToLeaf (Node name encounters nodeList) = Node name encounters [nodeToLeaf x | x <- nodeList]



-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair _ _ = (0, EmptyTree)

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree
mostEfficientSubtree dungeon = let tupleList = dungeonEfficiency dungeon
                                   (first_dungeon, eff) = head tupleList
                                   result = findTheMaxEff first_dungeon eff tupleList
                               in result
--helper functions of mostEfficientSubtree
--calculate the effects of every node and leaf individually
findTotalHpAndGold :: Dungeon -> [(Integer, Integer)]
findTotalHpAndGold (Leaf name encounters) = let (newHp, newGold) = findEncountersEffect 0 0 encounters
                                            in [(newHp, newGold)]
findTotalHpAndGold (Node name encounters []) = let (newHp, newGold) = findEncountersEffect 0 0 encounters
                                               in [(newHp, newGold)]
findTotalHpAndGold (Node name encounters nodeList) = let (newHp, newGold) = findEncountersEffect 0 0 encounters
                                                         list = concat [findTotalHpAndGold x | x <- nodeList]
                                                     in [(newHp, newGold)] ++ list
--turn the list into a tuple
hpAndGoldTuple :: Integer -> Integer -> [(Integer,Integer)] -> (Integer, Integer)
hpAndGoldTuple hp gold [] = (hp, gold)
hpAndGoldTuple hp gold (x:xs) = let increaseHp = fst x
                                    increaseGold = snd x
                                    newHp = hp + increaseHp
                                    newGold = gold + increaseGold
                                in hpAndGoldTuple newHp newGold xs
--find the efficiency
findTheEfficiency :: Dungeon -> Double
findTheEfficiency (Leaf name encounters) = let (newHp, newGold) = findEncountersEffect 0 0 encounters
                                           in if newHp >= 0 then let result = (fromInteger newGold) / (fromInteger (-1))
                                                                 in result
                                              else let result = (fromInteger newGold) / (fromInteger newHp)
                                                   in result
findTheEfficiency node@(Node name encounters nodeList) = let list = findTotalHpAndGold node 
                                                             tuplee = hpAndGoldTuple 0 0 list
                                                             (newHp, newGold) = tuplee
                                                         in if newHp >= 0 then let result = (fromInteger newGold) / (fromInteger (-1))
                                                                               in result
                                                            else let result = (fromInteger newGold) / (fromInteger newHp)
                                                                 in result
--make a list with efficiencies of all subtrees 
dungeonEfficiency :: Dungeon -> [(Dungeon, Double)]
dungeonEfficiency EmptyTree = [(EmptyTree, 0)]
dungeonEfficiency leaf@(Leaf name encounters) = let eff = findTheEfficiency leaf in [(leaf, eff)]
dungeonEfficiency node@(Node name encounters []) = let eff = findTheEfficiency node in [(node, eff)] 
dungeonEfficiency node@(Node name encounters nodeList) = let eff = findTheEfficiency node
                                                             list = concat [dungeonEfficiency x | x <- nodeList]
                                                         in [(node, eff)] ++ list


findTheMaxEff :: Dungeon -> Double -> [(Dungeon, Double)] -> Dungeon
findTheMaxEff dungeon n [] = dungeon
findTheMaxEff dungeon n (x:xs) = let newDungeon = fst x
                                     eff = snd x
                                 in if eff < n then findTheMaxEff newDungeon eff xs
                                    else findTheMaxEff dungeon n xs
