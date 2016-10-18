module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import qualified Data.Foldable hiding (concat)
import Data.Graph

set1 :: S.Set String
set1 = S.insert "welcome" $ S.singleton "hello"

set2 :: S.Set String
set2 = S.fromList ["hello","bye"]

setOps :: (S.Set String, Bool, S.Set Int)
setOps = ( set1 `S.intersection` set2
         , "welcome" `S.member` set1
         , S.map length set2 )

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) =
  let subtreesTraversed = concat $ map (preOrder f) subtrees
   in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]
                     

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = 
  [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
  ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
  ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]
                     
timeMachinePrecedence :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph
  
timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
                      [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                      ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]