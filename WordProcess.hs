-- Text manipulation
-- Supply: 1. an article, 2. a dependency parsed file
-- Will compute: 1. word - frequency map
--               2. word - word edge list
-- Edges will 1. start with the most frequent word
--            2. for each word, find a 3 level dependency
--            3. will visualize top 5 words to level 3 depth in the article

module WordProcess where

import Data.List
import Data.Char (toLower, isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import NLP.Tokenize
import Stopwords
import Test.HUnit 

edgeUnit = 1      -- edge length factor
numChildren = 5   -- maximum number of children of a word
maxDepth = 3      -- maximum number of generation of a word
maxMainNodes = 5  -- maximum source node of an article

------------------
-- | Dependency edge manipulation

-- parsed file processing
getAllEdgeStr :: Handle -> IO [String]
getAllEdgeStr h = do
                  flag <- hIsEOF h
                  if flag then return []
                  else do
                       l <- hGetLine h
                       ls <- getAllEdgeStr h
                       return (l:ls)

-- the top level funtion to get the edge list
-- as described at the top of this file
getAllEdges :: [String] -> Map String Float -> IO [(String, String)]
getAllEdges ls m = return $ 
                     processEdgeStr (processToGraph (parseEdgeStr ls m) m) m

-- separate each line in the parsed file to get the raw edge list
-- for each (w1, w2) pair, if one word is not in the weight map,
-- then the pair is discarded.
parseEdgeStr :: [String] -> Map String Float -> [(String, String)]
parseEdgeStr ls wm = filter (f wm) $ map parseEdge ls
  where parseEdge s = let sl = tokenize s
                          a = sl !! 0
                          b = sl !! 1
                      in (a, b)
        f m (a,b) = Map.member a m && Map.member b m

-- from the raw list, get all words dependent on a particular word
-- in the format of parent - children, children in descending order
-- for pair (w1, w2), if weight(w1) > weight(w2),
-- then w2 will be in the children list of w1 and NOT vice versa
processToGraph :: [(String, String)] -> Map String Float -> Map String [String]
processToGraph xs wm = Map.map (sortBy (sortFun wm)) (aux xs wm Map.empty)
  where aux [] _ r = r
        aux ((a,b):ps) m r =
          if m Map.! a >= m Map.! b then aux ps m (doKey a b r)
          else aux ps m (doKey b a r)
        doKey key val rm = if Map.member key rm
                           then Map.adjust (insertAct val) key rm
                           else Map.insert key [val] rm
        insertAct val ls = if notElem val ls then val:ls
                           else ls

-- expand a single word to max depth in breadth first fasion
-- initially, queue has a single parent
-- each depth, for a node, has 5 children in maximum
expandEdge :: [String] -> Int -> Map String [String] ->
              Set String -> [(String, String)]
expandEdge [] _ _ _ = []
expandEdge (e:queue) level m doneNodes
           = if level == maxDepth then []
             else if Map.member e m && Set.notMember e doneNodes
                  then makePairs e (m Map.! e)
                       ++ expandEdge (pushUniqQ queue (m Map.! e)) (level+1) m
                          (Set.insert e doneNodes)
             else expandEdge queue level m doneNodes

-- the main implementation of getAllEdges
-- taking a parent-child graph, parse it into edge list
-- in top frequency first and breadth first fashion
processEdgeStr :: Map String [String] -> Map String Float -> [(String, String)]
processEdgeStr edgeMp weights = aux edgeMp (sortMap weights) 0 []
  where aux _ [] _ result = result
        aux edges (node:nodes) level result
          = let parentSet = getParentSet result
            in if level == maxMainNodes then result
               else if Set.member node parentSet
                    then aux edges nodes level result
               else aux edges nodes (level+1) (result ++ 
                    expandEdge [node] 0 edges parentSet)

-- get all starting nodes
getParentSet :: [(String, String)] -> Set String
getParentSet ls = foldr (\(x,_) acc ->
                          if Set.member x acc then acc
                          else Set.insert x acc)
                          Set.empty ls

-- take a node and children list, parse into (node, child) pairs
-- limit of children is 5
-- before calling this function, it has been ensured that
-- children were sorted in descending frequency fashion
makePairs :: String -> [String] -> [(String, String)]
makePairs key ls = map (\x -> (key, x)) ch
  where ch = if length ls <= numChildren then ls
             else take numChildren ls


------------------
-- | Word frequency map

makeWeightMap :: String -> IO (Map String Float)
makeWeightMap s = return $ normalizeWeights $ generateWeights $ toWordList s

toWordList :: String -> [String]
toWordList str = filter wordFilter (map strToLower (tokenize str))
  where strToLower s = map toLower s

generateWeights :: [String] -> Map String Int
generateWeights strs = aux strs Map.empty
  where aux [] m = m
        aux (x:xs) m = if Map.member x m then aux xs (Map.adjust (+1) x m)
                       else aux xs (Map.insert x 1 m)

normalizeWeights :: Map String Int -> Map String Float
normalizeWeights m = Map.map (norm (maxWeight m)) m
  where maxWeight x = maximum $ Map.elems x
        norm n v = (fromIntegral v) / (fromIntegral n) * (fromIntegral edgeUnit)


------------------
-- | utilities

-- a word is elegible for our visualization would be:
-- 1. not in the stopword list provided, 2. length>=3,
-- 3. contains only letter and number
wordFilter :: String -> Bool
wordFilter x = (notElem x stopwords) && (length x >= 3) && (strIsAlphaNum x)
  where strIsAlphaNum s = foldr (\c acc -> acc && (isAlphaNum c)) True s

-- compare function of a list of words according to their weights
-- WARNING: in reverse order so that when sortBy is called,
-- the ordering is descending
sortFun :: Map String Float -> String -> String -> Ordering
sortFun m x y = let a = m Map.! x
                    b = m Map.! y
                in if a > b then LT
                   else if a < b then GT
                   else EQ

-- sort a map of words by their frequency
-- return the sorted list of keys
sortMap :: Map String Float -> [String]
sortMap m = sortBy (sortFun m) (Map.keys m)

-- taking a list as a queue with unique elements
pushUniqQ :: [String] -> [String] -> [String]
pushUniqQ queue elts = queue ++ (filter (\x -> notElem x queue) elts)


------------------
-- | unit testing
tProcessEdgeStr :: Test
tProcessEdgeStr = processEdgeStr (processToGraph testEdges testWeights)
                  testWeights ~?= [("Harry","Hermine"),("Harry","Ron"),("Harry","Voldemort"),("Harry","Dumbledore"),("Ron","Hermine"),("Ron","Ginny"),("Ron","Fred"),("Voldemort","Dumbledore")]

tExpandEdge :: Test
tExpandEdge = expandEdge ["Harry"] 0 (processToGraph testEdges testWeights)
              Set.empty ~?= [("Harry","Hermine"),("Harry","Ron"),("Harry","Voldemort"),("Harry","Dumbledore"),("Ron","Hermine"),("Ron","Ginny"),("Ron","Fred"),("Voldemort","Dumbledore")]

tProcessToGraph :: Test
tProcessToGraph = processToGraph testEdges testWeights 
                  ~?= Map.fromList [("Harry",["Hermine","Ron","Voldemort","Dumbledore"]),("Ron",["Hermine","Ginny","Fred"]),("Voldemort",["Dumbledore"])]

tGetParentSet :: Test
tGetParentSet = getParentSet [("A","B"),("A","C"),("B","D")]
                ~?= Set.fromList ["A","B"]

tMakePairs :: Test
tMakePairs = "makePairs" ~:
             makePairs "A" ["B","C","D","E","F"] ~?= [("A","B"),("A","C"),("A","D")]

tSortMap :: Test
tSortMap = "sortMap" ~:
           sortMap testWeights ~?= ["Harry","Hermine","Ron","Voldemort","Dumbledore","Malfoy","Snape","Ginny","Fred","George","Bellatrix","Lily"]

tPushUniqQ :: Test
tPushUniqQ = "pushUniqQ" ~:
            pushUniqQ ["Harry", "Ginny"] ["Ron","Harry","Lily","Ginny"]
            ~?=  ["Harry","Ginny","Ron","Lily"]

tSortFun :: Test
tSortFun = "sortFun" ~:
           sortBy (sortFun testWeights) ["Ron","Harry","Lily","Ginny"]
           ~?= ["Harry","Ron","Ginny","Lily"]


testWeights :: Map String Float
testWeights = Map.fromList
                [("Harry", 1), ("Ron", 0.9), ("Voldemort", 0.7),
                 ("Snape", 0.5), ("Hermine", 0.9), ("Dumbledore", 0.6),
                 ("Ginny", 0.4), ("Fred", 0.3), ("George", 0.3),
                 ("Bellatrix", 0.2), ("Malfoy", 0.5), ("Lily", 0.1)]

testEdges :: [(String, String)]
testEdges = [("Ron", "Ginny"),
             ("Harry", "Ron"),
             ("Harry", "Voldemort"),
             ("Harry", "Dumbledore"),
             ("Ron", "Fred"),
             ("Ron", "Hermine"),
             ("Harry", "Hermine"),
             ("Voldemort", "Dumbledore")]



