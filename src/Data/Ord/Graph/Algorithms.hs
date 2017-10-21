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
           #-}

module Data.Ord.Graph.Algorithms (
    Path, PathSegment(..),
    astar, dijkstra
    ) where

import           Control.Arrow
import           Control.Applicative hiding (empty)
import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.State
import           Control.Monad.Except

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

import           Prelude as P hiding (reverse)

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as G
import Data.List.NonEmpty (nonEmpty)
import Data.Ord.Graph


data AStarState i n = AStarState { _closedSet :: Set i
                                 , _openSet :: Set i
                                 , _cameFrom :: Map i i
                                 , _knownCost :: Map i n
                                 , _estimatedCost :: Map i n
                                 , _estimate :: (i -> n)
                                 }
makeLenses ''AStarState

data PathSegment i e v = PathNode i v
                       | PathEdge i i e
                       deriving (Show, Read, Data, Eq)

type Path i e v = [PathSegment i e v]
type AStarT i n a = StateT (AStarState i n) Maybe a

initialState :: (Ord i, Num cost, Ord cost) => i -> i -> (i -> i -> cost) -> AStarState i cost
initialState startIdx goal heuristic = AStarState
    { _closedSet = mempty
    , _openSet = S.singleton startIdx
    , _cameFrom = mempty
    , _knownCost = M.singleton startIdx 0
    , _estimatedCost = M.singleton startIdx $ heuristic startIdx goal
    , _estimate = flip heuristic goal
    }

-- Dijkstra is a special case of A* where the heuristic is 0 for all nodes
dijkstra :: (Ord i, Ord n, Num n)
      => (e -> n)      -- edge distance function
      -> Graph i e v   -- graph to search within
      -> i        -- start
      -> i        -- goal
      -> Maybe (Path i e v)
dijkstra = astar (\a b -> 0)

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
astar :: (Ord i, Ord n, Num n)
      => (i -> i -> n) -- node distance heuristic function
      -> (e -> n)      -- edge distance function
      -> Graph i e v   -- graph to search within
      -> i        -- start
      -> i        -- goal
      -> Maybe (Path i e v)
astar heuristic distance g start goal = do
    -- make sure these indices are actually in the graph
    a <- g ^? vertex start
    b <- g ^? vertex goal
    join . evalStateT go $ initialState start goal heuristic
    where
        go = do
            curr <- current
            if curr == goal
            then reconstructPath g curr `fmap` use cameFrom
            else do
                openSet   %= S.delete curr
                closedSet %= S.insert curr
                considerNeighboursOf curr
                go

        considerNeighboursOf idx = do
            cost <- costOf' idx 
            ns <- uses closedSet (openNeighbours g idx)
            mapM_ (considerNeighbour idx cost) ns

        considerNeighbour curr cost neighbour = do
            openSet %= S.insert neighbour
            nextCost <- lift $ (+ cost) <$> distanceBetween curr neighbour
            mknownCost <- costOf neighbour
            when (nextCost `isImprovementOn` mknownCost) $ record curr neighbour nextCost

        -- is in an improvement when the we have no known cost for the node, or the new cost is lower.
        isImprovementOn c = maybe True (c <)

        record prev idx cost = do
            est <- use estimate
            cameFrom      %= M.insert idx prev
            knownCost     %= M.insert idx cost
            estimatedCost %= M.insert idx (cost + est idx)

        costOf i = gets (M.lookup i . view knownCost)
        costOf' i = costOf i >>= lift -- skip on failure

        distanceBetween i j = distance <$> g ^? edge i j

openNeighbours :: (Ord i) => Graph i e v -> i -> Set i -> [i]
openNeighbours g idx closed = do
    filter (`S.notMember` closed) $ successors idx g

-- Given a graph, an end node, and a history of bread-crumbs,
-- build the path to that goal.
reconstructPath :: (Ord i) => Graph i e v -> i -> Map i i -> Maybe [PathSegment i e v]
reconstructPath g idx history = fmap intersperseEdges
                              $ sequence $ map (\i -> PathNode i <$> g ^? vertex i)
                              $ go history idx [idx]
    where intersperseEdges (PathNode i v : PathNode j v' : xs) = PathNode i v : concat [map (PathEdge i j) (g ^.. edge i j)
                                                                                       ,intersperseEdges (PathNode j v' : xs)]
          intersperseEdges xs = xs
          go h i is = case M.lookup i h of
                        Nothing -> is
                        Just next -> go h next (next : is)

-- current node, drawn from the open-set, ordered by estimated distance to goal
current :: (Ord i, Ord n) => AStarT i n i
current = do
    costs <- use estimatedCost
    lift =<< (gets $ fmap ( fst
                         . minimumBy (cmpCost `on` snd)
                         . fmap (\i -> (i, M.lookup i costs))
                         )
                  . nonEmpty
                  . S.toList
                  . view openSet)
    where
        cmpCost Nothing Nothing = EQ
        cmpCost Nothing       _ = GT
        cmpCost ma mb = fromMaybe LT $ liftA2 compare ma mb
