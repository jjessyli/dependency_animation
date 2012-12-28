-- Given  1. word - weight map
--        2. word - word edge list
-- Convert each word into a node
--         each edge into a Line between two nodes
-- Write out 1. evolve function
--           2. render function
-- evolve is to build the world model step by step from the nodes and edges
-- render is to render them into animation

module Animation where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Data.Map (Map)
import qualified Data.Map as Map

data WordStruct =
      WS { nodeName :: String -- word
         , nodeWeight :: Float -- weight, 0-1
         , nodePosition :: Point -- node's position
         , nodeFirstParent :: String -- first parent 
         , childrenIndex :: Int }  -- this node's index as a child
                     deriving Show
type Edges = [Picture]  -- Line 
type Nodes = [WordStruct]

data World = World Nodes Edges Int  
     deriving Show

-- | Given the word-weight map, find the word that has the largest weight
-- return a tuple of the word and weight
-- we can't use Map.findMax because it returns the key
findMaxWeight :: Map String Float -> (String, Float)
findMaxWeight mp = findMaxW_aux (Map.toList mp)

-- | This auxiliary function finds the best answer from tail to head
findMaxW_aux :: [(String, Float)] -> (String, Float)
findMaxW_aux [] = ("Nothing",0)
findMaxW_aux (a@(s_head,f_head):as)
              = let (s_tail,f_tail) = findMaxW_aux as
                in if f_head >= f_tail
                   then (s_head, f_head)
                   else (s_tail, f_tail)

-- | The translate distance of the center of the world from (0,0)
trans :: Float
trans = -60.0

-- | Build the initial world
-- The initial world only consists of one node
initWorld :: (String, Float) -> World
initWorld (word, weight) 
  = World [WS word weight (trans,trans) word 0] [] 0


-- | Given the word-word edge list, build the relationships between the words
-- For the Map k (a,b,c) that generates :
--  k is the node's name, a is his children's number 
--  b is his first parent's name, c is the node's index as a child
-- Special case: for an independent node, which has no parent,
--              its parent's itself and its index is 0 
wordRelation :: [(String, String)] ->
                Map String (Int, String, Int) ->
                Map String (Int, String, Int)
wordRelation [] mp = mp
wordRelation (e@(n1,n2) : es) mp = 
   case (Map.lookup n1 mp,Map.lookup n2 mp) of
        (Nothing, Nothing)         ->
         wordRelation es (Map.insert n2 (0,n1,1) (Map.insert n1 (1,n1,0) mp))
        (Just(i,j,k), Nothing)     -> 
         wordRelation es (Map.insert n2 (0,n1,(i+1)) (Map.insert n1 ((i+1),j,k) mp))
        (Nothing, Just(i,j,k))     -> 
         wordRelation es (Map.insert n1 (1,n1,0) mp)
        (Just(i,j,k), Just(l,m,n)) ->
         wordRelation es (Map.insert n1 ((i+1),j,k) mp)

-- | Given a list of words and a key word, find whether the key is in the list
findWord :: String -> Nodes -> Bool
findWord _ [] = False
findWord w (n:ns) = ((nodeName n) == w) || findWord w ns

-- | Return a word if it's in a list
returnWord :: String -> Nodes -> Maybe WordStruct
returnWord _ [] = Nothing
returnWord w (n:ns) = if nodeName n == w
                      then Just n
                      else returnWord w ns

-- | Given the word-weight map and the edge list
--  make it into the node(wordstruct) form
--  the Int argument is to count the independent node
wordToStruct :: Int -> Map String Float -> [(String, String)] ->
                Map String (Int, String, Int) -> Nodes -> Nodes
wordToStruct _ _ [] _ ns = ns
wordToStruct ct mp1 (e@(n1, n2) : es) mp2 ns 
 = if not (findWord n1 ns) && not (findWord n2 ns)
   then wordToStruct (ct + 1) mp1 es mp2 (ws2 : ws1 : ns)
   else if (findWord n1 ns) && (findWord n2 ns)
        then wordToStruct ct mp1 es mp2 ns
        else if (findWord n1 ns) && not (findWord n2 ns)
             then wordToStruct ct mp1 es mp2 (ws3 : ns)
             else wordToStruct (ct + 1) mp1 es mp2 (ws1 : ns)
               where (w1,w2) = case (Map.lookup n1 mp1, Map.lookup n2 mp1) of
                                    (Just a, Just b) -> (a,b)
                                    (Nothing, Nothing)-> (0,0)
                                    (Just a, Nothing) -> (a,0)
                                    (Nothing, Just a) -> (0,a)
                     (p1,p2,i1,i2)
                              = case (Map.lookup n1 mp2,Map.lookup n2 mp2) of
                                  (Just(_,f1,d1),Just(_,f2,d2)) -> (f1,f2,d1,d2)
                                  (_, _)       -> (n1,n2,0,0) --won't happen
                     ws1 = WS n1 w1 pos1 p1 i1
                     ws2 = WS n2 w2 pos2 p2 i2
                     ws3 = WS n2 w2 pos3 p2 i2
                     pos1 | ct == 0 = (trans,trans)
                          | otherwise = (fromIntegral 240 * cos alpha + trans,
                                        fromIntegral 240 * sin alpha + trans)
                     alpha = fromIntegral (ct - 1) * pi * 0.5 + 0.25 * pi
                     pos2  = parentpos2 +
                            (fromIntegral 110 * cos beta,
                             fromIntegral 110 * sin beta)
                     parentpos2
                           = case (returnWord p2 ((WS n1 w1 pos1 p1 i1):ns)) of 
                                 Just n  -> nodePosition n
                                 Nothing -> (1.0, 1.0) --won't happen
                     pos3 = parentpos3 + 
                            (fromIntegral (110 - (10 * i2)) * cos beta, 
                             fromIntegral (110 - (10 * i2) )* sin beta)
                     parentpos3 = case (returnWord p2 ns) of
                                   Just n  -> nodePosition n
                                   Nothing -> (1.0,1.0) --won't happen
                     beta = fromIntegral (i2 - 1) * pi * 0.4 + 0.3 * pi

edgeLines :: Map String Float -> [(String, String)] -> 
             Nodes -> [Picture] -> [Picture]
edgeLines _ [] _ pic = pic
edgeLines mp es@((n1, n2) : as) ns pic = edgeLines mp as ns (p : pic)
  where p = case (returnWord n1 ns, returnWord n2 ns) of
              (Just w1, Just w2) -> Line [nodePosition w1, nodePosition w2]
              (_, _)             -> Line [(0,0),(0,0)]  --won't happen


render :: World -> Picture
render (World nodes edges steps) =
              Color (makeColor 0.3 0.3 0.3 1.0)
            $ Pictures ((map renderNode nodes) ++ edges)

renderNode :: WordStruct -> Picture
renderNode ws@(WS word weight pos@(x,y) parent index) 
   = let z     = fromIntegral index * 0.17
         color = makeColor 0.6 z 0.7 1.0        --node's color
         ratio = 0.5 * (weight + 0.2) / (1.0 + weight)
     in
        Color color
              $ Translate x y
              $ Scale ratio ratio
              $ Text word


evolve ::  Map String Float -> [(String,String)] ->
           ViewPort -> Float -> World -> World
evolve  weights tedges vp step world@(World nodes edges steps) 
       | steps < length tedges
       = expandNet world (tedges !! steps) weights tedges 
       | otherwise
       = world

expandNet :: World -> (String, String) ->
             Map String Float -> [(String, String)] -> World
expandNet w@(World nodes edges steps) e@(n1,n2) weights tedges
 = World newnodes newedges (steps + 1)
    where newnodes 
           = let c1 = returnWord n1 nodes
                 c2 = returnWord n2 nodes
                 c3 = returnWord n1 nodesList
                 c4 = returnWord n2 nodesList
             in case (c1, c2, c3, c4) of
               (Just _, Just _, Just _, Just _) -> nodes
               (Nothing,Just _, Just w1, Just _) -> (w1:nodes) 
               (Just _,Nothing, Just _, Just w2) -> (w2:nodes)
               (Nothing,Nothing,Just w1, Just w2) -> (w1:w2:nodes)
          newedges = (edgesList !! steps) : edges
          nodesList = reverse (wordToStruct 0 weights tedges 
                      (wordRelation tedges Map.empty) [])   
          edgesList = reverse (edgeLines weights tedges nodesList [])                            
