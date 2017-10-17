{-# LANGUAGE TupleSections, RankNTypes #-}

-- Helpers for constructing fully connected two-dimensional grids
-- which are very easy to visualise and reason about for path finding
module Data.Ord.Lattice (Lattice(..), lattice, printLattice) where

import Prelude hiding (reverse)

import Control.Monad
import Data.Monoid
import Control.Lens hiding (elements)
import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import qualified Data.Ord.Graph as G
import           Data.Ord.Graph.Algorithms 

newtype Lattice = Lattice { unlattice :: G.Graph (Int, Int) String String }
                deriving (Show, Eq)

instance Arbitrary Lattice where
    arbitrary = do
        w <- arbitrary `suchThat` (> 0)
        h <- arbitrary `suchThat` (> 0)
        return $ Lattice (lattice w h)

data Edge = Missing | Bidirectional | Upwards | Downwards

printLattice :: G.Graph (Int, Int) String String -> IO ()
printLattice g = do
    let inds = G.idxs g
        zero = (0, 0)
        (maxX, maxY)  = maximum inds
    forM_ [-1 .. maxY] $ \y -> do
        printVerticalRow maxX y
        printHorizontalRow maxX y
    where
        printVerticalRow mx y = do
            let edges = map (\x -> edgeAt x y) [0 .. mx]
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
        edgeAt x y = let upwards = has (G.edge (x, y) (x, pred y)) g
                         downwards = has (G.edge (x, pred y) (x, y)) g
                     in case (upwards, downwards) of
                        (True, True) -> Bidirectional
                        (True, False) -> Upwards
                        (False, True) -> Downwards
                        (False, False) -> Missing
        edgesToLine es = unlines [ ' ' : (L.intercalate "   " $ map (fromEdge Upwards) es)
                                 , ' ' : (L.intercalate "   " $ map (fromEdge Bidirectional) es)
                                 , ' ' : (L.intercalate "   " $ map (fromEdge Downwards) es)
                                 ]

        nodesToLine [] = []
        nodesToLine ((l, r, n) : ns) = concat [ lhs l
                                           , if n then "*" else " "
                                           , rhs r
                                           , nodesToLine ns
                                           ]
        lhs (Just True) = ">"
        lhs (Just False) = "-"
        lhs Nothing      = " "
        rhs (Just True) = "<-"
        rhs (Just False) = "--"
        rhs Nothing      = "  "

        fromEdge _       Missing = " "
        fromEdge Upwards Upwards = "^"
        fromEdge Upwards Bidirectional = "^"
        fromEdge Downwards Downwards = "v"
        fromEdge Downwards Bidirectional = "v"
        fromEdge _ _ = "|"

lattice :: Int -> Int -> G.Graph (Int, Int) String String
lattice w h = G.fromLists (map (\pos -> (pos, showv pos)) vs)
                          (map (\(a, b) -> (a, b, showe (a, b))) es)
    where 
        showv (x, y) = show x <> "-" <> show y
        showe (a, b) = show a <> "->" <> show b
        vs = [(x, y) | x <- [0 .. w], y <- [0 .. h]]
        es = concatMap linkToNeighbours vs
        linkToNeighbours here = let (x, y) = here
                                  in map (here,)
                                     $ filter (\(x, y) -> x >= 0 && x <= w && y >= 0 && y <= h)
                                     $ [(pred x, y), (x, pred y), (succ x, y), (x, succ y)]
