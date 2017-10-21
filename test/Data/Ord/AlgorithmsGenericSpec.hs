{-# LANGUAGE TupleSections, RankNTypes #-}
module Data.Ord.AlgorithmsGenericSpec (spec) where

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
import           Data.Ord.Graph.AlgorithmsGeneric
import           Data.Ord.Lattice

spec :: Spec
spec = describe "GenericAlgorithms" $ do
    let chain :: G.Graph Int String Char
        chain = G.fromLists (zip [0 ..] "abcdef")
                            [(0, 1, "e1")
                            ,(1, 2, "e2")
                            ,(2, 3, "e3")
                            ,(3, 4, "e4")
                            ,(4, 5, "e5")
                            ]
        shortcut = G.fromLists [(1, 'b'), (100, 'x'), (101, 'y'), (5, 'f')]
                               [(1,   100, "sc0")
                               ,(100, 101, "sc1")
                               ,(101, 5,   "sc2")
                               ]
        dij = dijkstra (const 1)
    it "can follow a very simple path" $ do
        let g = chain
            Just path = dij g 0 5
        edges path `shouldBe` ["e1", "e2", "e3", "e4", "e5"]
    it "Cannot find links to items not in the chain" $ do
        let mpath = dij chain 0 100
        mpath `shouldBe` Nothing
    it "Can find a shorter path, when available" $ do
        let g = chain <> shortcut
            Just path = dij g 0 5
        edges path `shouldBe` ["e1", "sc0", "sc1", "sc2"]

    describe "lattices" $ do
        it "finds any index within an aribitray lattice" $ property $ \(LatticeBounds x y pos) ->
            S.member pos (G.idxSet . unlattice $ lattice x y)
        it "finds any edge with an arbitrary lattice" $ property $ \(LatticeWithEdge w h (i, j)) ->
            G.edge i j `has` (unlattice $ lattice w h)
        let Lattice g = lattice 5 4
            go = dijkstra id g
        it "allow any node to be reached from any other" $ property $ \(PathSearchInput (Lattice g) start end) ->
            maybe False (not . null) $ astar cartesianDistance id g start end
        it "Can go horizontally" $ do
            path <- dijkstra (const 1) g (0, 1) (4, 1)
            length (nodes path) `shouldBe` 5
        it "Can go vertically" $ do
            let Just path = go (1, 0) (1, 3)
            length (nodes path) `shouldBe` 4
        it "Can go diagonally" $ do
            path <- astar cartesianDistance id g (0,0) (4, 3)
            length (edges path) `shouldBe` 4
        let withhole = removeNodes [ (2, 0)
                                   , (2, 1)
                                   , (3, 1)
                                   , (3, 2)
                                   ] g
        it "Can still go horizontally, but the long way around" $ do
            path <- astar cartesianDistance (const 1) withhole (0, 1) (4, 1)
            length (edges path) `shouldBe` 5

nodes :: Path i e v -> [v]
nodes = concatMap f
    where f (PathNode _ v) = [v]
          f _ =  []

edges :: Path i e v -> [e]
edges = concatMap f
    where f (PathEdge _ _ e) = [e]
          f _ = []

data LatticeBounds = LatticeBounds Int Int (Int, Int) deriving (Show, Eq)

data LatticeWithEdge = LatticeWithEdge Int Int ((Int, Int), (Int, Int)) deriving (Show, Eq)

instance Arbitrary LatticeBounds where
    arbitrary = do
        w <- positiveInts
        h <- positiveInts
        x <- choose (0, pred w)
        y <- choose (0, pred h)
        return $ LatticeBounds w h (x, y)

instance Arbitrary LatticeWithEdge where
    arbitrary = do
        LatticeBounds w h (x, y) <- arbitrary
        pos <- elements [(pred x, y), (succ x, y)
                        ,(x, pred y), (x, succ y)
                        ] `suchThat` (\(x', y') -> x' >= 0 && x' <= w && y' >= 0 && y' <= h)
        return $ LatticeWithEdge w h ((x, y), pos)

positiveInts :: Gen Int
positiveInts = arbitrary `suchThat` (> 0)

data PathSearchInput = PathSearchInput Lattice (Int, Int) (Int, Int)
                deriving (Show, Eq)

instance Arbitrary PathSearchInput where
    arbitrary = do
        Lattice g <- arbitrary
        let indices = G.idxs g
        PathSearchInput (Lattice g) <$> elements indices
                                    <*> elements indices
                          
