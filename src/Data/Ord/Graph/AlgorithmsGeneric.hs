{-# LANGUAGE TemplateHaskell
           , DeriveDataTypeable
           , TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses
           , PatternSynonyms
           , ViewPatterns
           , RankNTypes
           , NoMonomorphismRestriction
           , FunctionalDependencies
           , ViewPatterns
           #-}

module Data.Ord.Graph.AlgorithmsGeneric (
    Path, PathSegment(..),
    astarGeneric, astar, dijkstra, cartesianDistance,
    SearchState(..)
    ) where

import Debug.Trace
import           Control.Arrow
import           Control.Applicative hiding (empty)
import           Control.Lens hiding (Index)
import           Control.Monad (join)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Fail (MonadFail)

import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Semigroup
import           Data.Data (Data)
import           Data.Function (on)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.List (partition, minimumBy)
import           Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.PriorityQueue.FingerTree as PQ

import           Prelude as P hiding (reverse)

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as G

import Data.Ord.Graph
import Data.Ord.Graph.Algorithms.SearchState
import Data.Ord.Graph.Algorithms.PriorityQueueState

data PathSegment i e v = PathNode i v
                       | PathEdge i i e
                       deriving (Show, Read, Data, Eq)

type Path i e v = [PathSegment i e v]

-- Dijkstra is a special case of A* where the heuristic is 0 for all nodes
dijkstra :: (MonadFail m, Ord i, Ord n, Num n)
      => (e -> n)      -- edge distance function
      -> Graph i e v   -- graph to search within
      -> i        -- start
      -> i        -- goal
      -> m (Path i e v)
dijkstra = astar constantEstimate

-- Breadth-first Search with estimated and exact cost functions
-- To use this correctly, make sure that:
-- * Your graph has edges annotated with their length (also known as weight), or where their
--   weight can be calculated in a simple function. A good edge might be:
--     data Edge = Edge { edgeName :: String, edgeWeight :: Int }
-- * You can estimate a distance between any vertex and the goal based on their properties. If this is not
--   possible, you can simply return 0, but a better option might be to include absolute position, and
--   use this to calculate an estimated distance, eg:
--     data Node = Node { nodePosition :: (Int, Int) }
--     let distance a b = absoluteDistance (nodePosition a) (nodePosition b)
--  Currently these graphs only allow a single directed edge between any two vertices - it is not possible
--  to have two edges with different weights between the same two nodes. You can fake that with synthetic nodes
--  if required.
astar :: (Ord i, Ord n, Num n, MonadFail m)
      => (i -> i -> n) -- node distance heuristic function
      -> (e -> n)      -- edge distance function
      -> Graph i e v   -- graph to search within
      -> i        -- start
      -> i        -- goal
      -> m (Path i e v)
astar heuristic distance g start goal = do
    -- make sure these indices are actually in the graph
    state <- case (g ^? vertex start, g ^? vertex goal) of
        (Nothing, _) -> fail "Start index not found in graph"
        (_, Nothing) -> fail "End index not found in graph"
        _            -> return $ priorityQueueState start goal heuristic distance
    evalStateT (astarGeneric g) state

astarGeneric :: (MonadFail m, SearchState s m) => Graph (Index s) (Edge s) v -> StateT s m (Path (Index s) (Edge s) v)
astarGeneric g = go
    where
        go = do
            finished <- isAtGoal
            if finished
                then reconstructPath g <$> indicesToHere
                     >>= maybe (lift $ fail "Could not reconstruct path") return
                else considerNeighbours >> markClosed >> advance >> go

        considerNeighbours = do
            curr <- currentIdx
            cost <- costToHere
            ns <- openNeighbours g curr <$> isClosed
            mapM_ (considerNeighbour curr cost) ns

        considerNeighbour curr cost neighbour = do
            case g ^? edge curr neighbour of
                Nothing -> fail "Could not find edge"
                Just e -> distanceAlong e >>= enqueue neighbour

openNeighbours :: (Ord i) => Graph i e v -> i -> (i -> Bool) -> [i]
openNeighbours g idx closed = filter (not . closed) $ successors idx g

-- Given a graph and a path described as a list of indices,
-- build the path to that goal.
reconstructPath :: (Ord i) => Graph i e v -> [i] -> Maybe (Path i e v)
reconstructPath g = -- maybe (fail "Could not reconstruct path") return
                    fmap intersperseEdges
                  . sequence
                  . map (\i -> PathNode i <$> g ^? vertex i)
    where intersperseEdges (PathNode i v : PathNode j v' : xs)
            = PathNode i v : concat [ map (PathEdge i j) (g ^.. edge i j)
                                    , intersperseEdges (PathNode j v' : xs)]
          intersperseEdges xs = xs

constantEstimate :: (Num n) => a -> a -> n
constantEstimate _ _ = 0

class (Real n) => CartesianCoordinate x n | x -> n where
    xy :: x -> (n, n)

instance (Real n) => CartesianCoordinate (n, n) n where
    xy = id
    
-- general purpose function for computing a straight-line distance
-- between two points on a cartesian plane
cartesianDistance :: (CartesianCoordinate c n, Real n, Floating b) => c -> c -> b
cartesianDistance (xy -> (x0, y0)) (xy -> (x1, y1)) = sqrt $ dx ^ 2 + dy ^ 2
    where dx = realToFrac . abs $ x1 - x0
          dy = realToFrac . abs $ y1 - y0
