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
           #-}

module Data.Ord.Graph.Algorithms.PriorityQueueState (
    priorityQueueState, PriorityQueueState
    ) where

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
import qualified Data.PriorityQueue.FingerTree as PQ

import Data.Ord.Graph.Algorithms.SearchState

data PriorityQueueState e i n
    = PQState { _currentIndex :: i
              , _currentCost :: n
              , _targetIndex :: i
              , _priorityQueue :: PQ.PQueue n i
              , _closedSet :: Set i
              , _cameFrom  :: Map i (PQ.PQueue n i)
              , _estimate  :: (i -> n)
              , _measure   :: (e -> n)
              }
makeLenses ''PriorityQueueState

priorityQueueState :: (Ord i, Ord n, Num n) => i -> i -> (i -> i -> n) -> (e -> n) -> PriorityQueueState e i n
priorityQueueState start goal heuristic edgeDistance = PQState
    { _currentIndex = start
    , _targetIndex = goal
    , _currentCost = 0
    , _priorityQueue = mempty
    , _closedSet = mempty
    , _cameFrom = mempty
    , _estimate = (flip heuristic goal)
    , _measure = edgeDistance
    }

instance (Num n, Ord n, Ord i, MonadFail m) => SearchState (PriorityQueueState e i n) m where
    type Index (PriorityQueueState e i n) = i
    type Cost  (PriorityQueueState e i n) = n
    type Edge  (PriorityQueueState e i n) = e
    isAtGoal = (==) <$> use currentIndex <*> use targetIndex
    costToHere = use currentCost
    currentIdx = use currentIndex

    advance = do
        ((c, i), q) <- uses priorityQueue PQ.minViewWithKey
                       >>= maybe (fail "No path found") return
        priorityQueue .= q
        closed      <- uses closedSet (S.member i)
        if closed
          then advance
          else currentIndex .= i >> currentCost .= c >> return (i, c)

    markClosed    = do i <- currentIdx
                       closedSet %= S.insert i

    isClosed      = uses closedSet (flip S.member)
    distanceAlong e = uses measure ($ e)

    enqueue i c = do
        est <- (c +) <$> uses estimate ($ i)
        priorityQueue %= PQ.add est i
        curr <- use currentIndex
        cameFrom . at i %= (Just . maybe (PQ.singleton c curr) (PQ.add c curr))

    indicesToHere = go [] <$> use cameFrom <*> currentIdx
        where go acc h i = let acc' = (i : acc) in case (h ^? ix i) >>= PQ.minView of
                Nothing        -> acc'
                Just (next, _) -> go acc' h next 

