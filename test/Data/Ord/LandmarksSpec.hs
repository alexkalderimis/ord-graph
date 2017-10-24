module Data.Ord.LandmarksSpec (spec) where

import Prelude hiding (reverse)

import Data.Function (on)
import Control.Lens hiding (elements)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import qualified Data.Ord.Graph as G
import           Data.Ord.Graph.AlgorithmsGeneric (euclideanDistance)
import           Data.Ord.Graph.Algorithms.Landmarks
import           Data.Ord.Lattice

type Distances = HashCache Int Float
type Landmarks = ([(Int, Int)], Distances)

spec :: Spec
spec = describe "Landmarks" $ do
    let shouldContainSame = shouldBe `on` S.fromList

    describe "selectLandmarks" $ do
        describe "line" $ do
            let line =  unlattice $ lattice 4 0
                landmarks :: Landmarks
                landmarks = selectLandmarks 3 (2, 0) line id euclideanDistance 
            it "can select the ends of the line" $ do
                printLattice line
                fst landmarks `shouldContainSame`  [(2, 0) -- seed, plus points
                                                   ,(0, 0), (4, 0)
                                                   ]

        describe "star" $ do
            let removed =[(x,y) | x <- [0, 2, 4], y <- [0, 2, 4], (x,y) /= (2, 2)]
                onEdge (x, y) = x `elem` [0, 4] || y `elem` [0, 4]
                star = G.imapEdges (\i j -> if onEdge i || onEdge j then (* 3) else id)
                     $ removeNodes removed
                     $ unlattice $ lattice 4 4
                landmarks :: Landmarks
                landmarks = selectLandmarks 9 (2, 2) star id euclideanDistance 
                d = heuristicWithLandmarksMap (snd landmarks) (fst landmarks)
            it "can select the points of the star" $ do
                printLattice star
                fst landmarks `shouldContainSame`  [(2, 2) -- seed, plus points
                                            ,(1,0), (3, 0)
                                            ,(0,1), (4, 1)
                                            ,(0,3), (4, 3)
                                            ,(1,4), (3, 4)
                                            ]
            it "knows distances that are better than or equal to the estimates"
               $ property
               $ forAll ((,) <$> elements (fst landmarks) <*> elements (G.idxs star))
               $ \(v, t) -> d v t >= euclideanDistance v t
