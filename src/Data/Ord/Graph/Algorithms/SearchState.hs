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

module Data.Ord.Graph.Algorithms.SearchState (SearchState(..)) where

import           Control.Monad.Fail (MonadFail)
import           Control.Monad.State

-- interface of a state we can use to search within a graph
-- You might want to implement your own search state for several reasons:
--  * To implement different search behaviour (eg. dynamic search, where the
--    goal can change during the search)
--  * To improve efficiency (such as using hash storage for hashable indices)
--  * Performance, such as avoiding allocation with mutable mappings in ST
class (MonadFail m, Ord (Index s)) => SearchState s m where
    type (Index s) :: *
    type (Cost s) :: *
    type (Edge s) :: *
    advance    :: StateT s m (Index s, Cost s)
    isAtGoal   :: StateT s m Bool
    costToHere :: StateT s m (Cost s)
    currentIdx :: StateT s m (Index s)
    markClosed :: StateT s m ()
    isClosed   :: StateT s m (Index s -> Bool)
    enqueue    :: Index s -> Cost s -> StateT s m ()
    distanceAlong :: Edge s -> StateT s m (Cost s)
    indicesToHere :: StateT s m [Index s]

