{-# LANGUAGE TypeFamilies
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
import           Criterion.Main

import           Control.Monad.Fail (MonadFail)
import           Control.Monad.State
import           Data.Hashable
import qualified Data.Ord.Graph as G
import qualified Data.Ord.Graph.AlgorithmsGeneric as AG
import qualified Data.Ord.Graph.Algorithms as A
import           Data.Ord.Graph.Algorithms.HashableQueueState (hashableQueueState)
import           Data.Ord.Lattice

type Idx = (Int, Int)
type Edge = Float
type UnLattice = G.Graph Idx Edge String

-- a lattice with a couple of bars in the middle. Not a massive obstacle,
-- just enough to make sure we don't make things too easy.
zigzag :: Int -> Int -> Lattice
zigzag w h = Lattice . removeNodes obstructions . unlattice $ lattice w h
    where obstructions = lbar ++ rbar
          top    = 1 / 5 :: Float
          bottom = 4 / 5 :: Float
          left   = 1 / 4 :: Float
          right  = 3 / 4 :: Float
          ys   = [floor (fromIntegral h * top) .. floor (fromIntegral h * bottom)]
          lbar = [(x, y) | x <- [floor (fromIntegral w * left)] , y <- ys ]
          rbar = [(x, y) | x <- [floor (fromIntegral w * right)] , y <- ys ]
                        
astarHashable :: (Ord i, Hashable i, Eq i, Ord n, Num n, MonadFail m)
              => (i -> i -> n)
              -> (e -> n)
              -> G.Graph i e v
              -> i -> i -> m (AG.Path i e v)
astarHashable h d g i j = let s = hashableQueueState i j h d
    in evalStateT (AG.astarGeneric g) s

main :: IO ()
main = do
    let
        cd :: Idx -> Idx -> Float
        cd = AG.cartesianDistance

        (Lattice s) = zigzag 10 10
        (Lattice m) = zigzag 100 100
        (Lattice l) = zigzag 250 250
        (Lattice xl) = zigzag 500 500

        a :: UnLattice -> Idx -> Idx -> Maybe (A.Path Idx Edge String)
        a  = s `seq` m `seq` l `seq` xl `seq` A.astar cd id
        ag :: UnLattice -> Idx -> Idx -> Maybe (AG.Path Idx Edge String)
        ag = a `seq` AG.astar cd id
        hg :: UnLattice -> Idx -> Idx -> Maybe (AG.Path Idx Edge String)
        hg = ag `seq` astarHashable cd id

        zero = (0, 0)
        gs = [ (n, g, maximum $ G.idxs g)
             | (n, g) <- [("small", s), ("medium", m), ("large", l), ("xl", xl)]]
        benches f = [bench name (whnf (f g zero) i) | (name, g, i) <- gs]

    -- check that we can traverse each graph
    printLattice s -- take a look at the small one
    defaultMain
        [ bgroup "data.map" (benches a)
        , bgroup "fingertrees" (benches ag)
        , bgroup "hashable" (benches hg)
        ]
