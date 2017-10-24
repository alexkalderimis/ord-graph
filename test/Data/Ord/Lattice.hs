{-# LANGUAGE TupleSections, RankNTypes #-}

-- Helpers for constructing fully connected two-dimensional grids
-- which are very easy to visualise and reason about for path finding
module Data.Ord.Lattice (Lattice(..), lattice, printLattice, removeNodes) where

import Prelude hiding (reverse)

import Control.Monad
import Data.Monoid
import Control.Lens hiding (elements)
import Test.QuickCheck

import qualified Data.Ord.Graph as G

newtype Lattice = Lattice { unlattice :: G.Graph (Int, Int) Float String }
                deriving (Show, Eq)

instance Arbitrary Lattice where
    arbitrary = let dim = arbitrary `suchThat` (> 0) `suchThat` (< 50)
                in lattice <$> dim <*> dim

data Edge = Missing | Bidirectional | Upwards | Downwards | Diag Bool Bool Bool Bool

-- for debugging and development
printLattice :: G.Graph (Int, Int) Float String -> IO ()
printLattice g = do
    let inds = G.idxs g
        (maxX, maxY)  = maximum inds
    forM_ [-1 .. succ maxY] $ \y -> do
        printVerticalRow maxX y
        printHorizontalRow maxX y
    where
        printVerticalRow _ y | y <= 0 = return ()
        printVerticalRow mx y = do
            let edges = concatMap (\x -> edgeAt x y) [0 .. mx]
            putStr $ edgesToLine edges
        printHorizontalRow mx y = do
            let nodes = map (\x -> nodeAt x y) [0 .. mx]
            putStrLn $ nodesToLine nodes
        nodeAt x y = let incomingFromL = G.edge (pred x, y) (x, y) `has` g
                         outgoingToL   = G.edge (x, y) (pred x, y) `has` g
                         incomingFromR = G.edge (succ x, y) (x, y) `has` g
                         outgoingToR   = G.edge (x, y) (succ x, y) `has` g
                         exists        = G.vertex (x, y) `has` g
                         incoming inc out = case (inc, out) of
                                (False, False) -> Nothing
                                (True,  _)     -> Just True
                                (False, True)  -> Just False

                     in (incoming incomingFromL outgoingToL, incoming incomingFromR outgoingToR, exists)
        edgeAt x y = let here = (x, y)
                         above = (x, pred y)
                         upwards = has (G.edge here above) g
                         downwards = has (G.edge above here) g
                         tl = has (G.edge (succ x, y) above) g
                         tr = has (G.edge here (succ x, pred y)) g
                         br = has (G.edge above (succ x, y)) g
                         bl = has (G.edge (succ x, pred y) here) g
                         ve = case (upwards, downwards) of
                                (True, True) -> Bidirectional
                                (True, False) -> Upwards
                                (False, True) -> Downwards
                                (False, False) -> Missing
                     in [ve, Diag tl tr br bl]
        edgesToLine es = unlines [ ' ' : concatMap (fromEdge Upwards) es
                                 , ' ' : concatMap (fromEdge Bidirectional) es
                                 , ' ' : concatMap (fromEdge Downwards) es
                                 ]

        nodesToLine [] = []
        nodesToLine ((l, r, n) : ns) = concat [ lhs l
                                           , if n then "▪" else " "
                                           , rhs r
                                           , nodesToLine ns
                                           ]
        lhs (Just True) = "→"
        lhs (Just False) = "-"
        lhs Nothing      = " "
        rhs (Just True) = "←-"
        rhs (Just False) = "--"
        rhs Nothing      = "  "

        fromEdge Upwards   (Diag tl tr tl' tr') = corner '┌' '╲' tl tl' : ' ' : corner '┐' '╱' tr tr' : []
        fromEdge Downwards (Diag br' bl' br bl) = corner '└' '╱' bl bl' : ' ' : corner '┘' '╲' br br' : []
        fromEdge Bidirectional (Diag tl tr tl' tr') = ' ' : case (tl || tl', tr || tr') of
                                                                (True, True) -> 'x'
                                                                (True, False) -> '╲'
                                                                (False, True) -> '╱'
                                                                _             -> ' '
                                                          : " "

        fromEdge _       Missing = " "
        fromEdge Upwards Upwards = "↑"
        fromEdge Upwards Bidirectional = "↑"
        fromEdge Downwards Downwards = "↑"
        fromEdge Downwards Bidirectional = "↓"
        fromEdge _ _ = "|"

        corner _ _ False False = ' '
        corner a _ True _ = a
        corner _ t _ True = t

lattice :: Int -> Int -> Lattice
lattice w h = Lattice $ G.fromLists (map (\pos -> (pos, showv pos)) vs) es
    where 
        straight = 1
        diagonal = sqrt 2
        showv (x, y) = show x <> "-" <> show y
        vs = [(x, y) | x <- [0 .. w], y <- [0 .. h]]
        es = concatMap linkToNeighbours vs
        inBounds i x y = (x, y) /= i && x >= 0 && x <= w && y >= 0 && y <= h
        linkToNeighbours i@(x, y) = map (\(x', y', d) -> (i, (x', y'), d))
                                    $ filter (\(v, v', _) -> inBounds i v v')
                                    $ [ (pred x, y,      straight), (succ x, y,      straight) -- L/R
                                      , (x,      pred y, straight), (x,      succ y, straight) -- T/B
                                      , (pred x, pred y, diagonal), (succ x, succ y, diagonal) -- TL/BR
                                      , (succ x, pred y, diagonal), (pred x, succ y, diagonal) -- BL/TR
                                      ]
        
removeNodes :: (Ord i) => [i] -> G.Graph i e v -> G.Graph i e v
removeNodes is g = foldr G.delIdx g is
