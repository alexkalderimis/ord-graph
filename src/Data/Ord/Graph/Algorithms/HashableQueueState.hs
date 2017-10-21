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

module Data.Ord.Graph.Algorithms.HashableQueueState (
    hashableQueueState, HashableQueueState
    ) where

import           Control.Arrow
import           Control.Applicative hiding (empty)
import           Control.Lens hiding (Index)
import           Control.Monad (join)
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Fail (MonadFail)

import           Data.Hashable
import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Semigroup
import           Data.Data (Data)
import           Data.Function (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.PriorityQueue.FingerTree as PQ

import Data.Ord.Graph.Algorithms.SearchState

type Map = HashMap
type Set = HashSet

data HashableQueueState e i n
    = HQState { _currentIndex :: i
              , _currentCost :: n
              , _targetIndex :: i
              , _priorityQueue :: PQ.PQueue n i
              , _closedSet :: Set i
              , _cameFrom  :: Map i (PQ.PQueue n i)
              , _estimate  :: (i -> n)
              , _measure   :: (e -> n)
              }
makeLenses ''HashableQueueState

-- identical to priorityQueueState, except for for the hashable constraint
hashableQueueState :: (Hashable i, Eq i, Ord n, Num n) => i -> i -> (i -> i -> n) -> (e -> n) -> HashableQueueState e i n
hashableQueueState start goal heuristic edgeDistance = HQState
    { _currentIndex = start
    , _targetIndex = goal
    , _currentCost = 0
    , _priorityQueue = mempty
    , _closedSet = mempty
    , _cameFrom = mempty
    , _estimate = (flip heuristic goal)
    , _measure = edgeDistance
    }

-- complete duplication of PriorityQueueState instance, which could be abstracted with another type class
-- with lenses as members, but that is an inane level of indirection
instance (Num n, Hashable i, Eq i, Ord n, Ord i, MonadFail m) => SearchState (HashableQueueState e i n) m where
    type Index (HashableQueueState e i n) = i
    type Cost  (HashableQueueState e i n) = n
    type Edge  (HashableQueueState e i n) = e
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

