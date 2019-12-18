module Util.AStar(aStarSearch, genAStarSearch) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List hiding (insert)
import Data.Function
import Data.PQueue.Min

-- data types for the A* search
-- node stores f value (cost + heuristic), cost to get there and value
data Node a = Node {
  getF :: !Int,
  getC :: !Int,
  getVal :: !a
} deriving (Eq, Ord)
-- successor function with costs
type SuccFun a = a -> Set (a, Int)
-- heuristic for the problem
type Heuristic a = a -> Int
-- function which checks if a goal was reached
type GoalTest a = a -> Bool

-- A* search returning length of minimal path to solution, with a single final state
aStarSearch :: (Ord a) => a -> a -> Heuristic a -> SuccFun a -> Int
aStarSearch initial final h succFun = fst (runAStar h succFun (== final) open Set.empty) where
  open = singleton $ Node {getF = (h initial), getC = 0, getVal = initial}
-- A* search returning length of minimal path to solution and final state reached. Allows several final states
genAStarSearch :: (Ord a) => a -> Heuristic a -> SuccFun a -> GoalTest a -> (Int, a)
genAStarSearch initial h succFun goal = runAStar h succFun goal open Set.empty where
  open = singleton $ Node {getF = (h initial), getC = 0, getVal = initial}
    
-- run A*, returning cost of best solution and the final state reached
runAStar :: (Ord a) => Heuristic a -> SuccFun a -> GoalTest a -> MinQueue (Node a) -> Set a -> (Int, a)
runAStar h succFun goal open closed = 
    let Node f c next = findMin open in
    -- check for goal
      if goal next  
        then (c, next)
        -- check if node already visited
        else if elem next closed
            then runAStar h succFun goal (deleteMin open) closed
            else
                -- add to closed list
                let newClosed = Set.insert next closed
                -- add successors to open list
                    successors = [(s, i) | (s, i) <- Set.elems (succFun next), not $ Set.member s closed]
                    newOpen = foldl (\op new -> insert (newNode new) op) (deleteMin open) successors
                    newNode (newS, cost) = Node {getF = fNew (newS, cost), getC = c + cost, getVal = newS}
                    -- f value for successors, with path max correction
                    fNew (newS, cost) = max (c + cost + (h newS)) f
                  -- continue
                  in runAStar h succFun goal newOpen newClosed