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
           , AllowAmbiguousTypes
           , ScopedTypeVariables
           #-}

-- Helpers to create and use landmarks.
-- You don't have to use these functions to use landmarks, but they may
-- be helpful when creating them/using them to improve your heuristic functions.
module Data.Ord.Graph.Algorithms.Landmarks where

import           Control.Parallel.Strategies (withStrategy, parList, rseq, rpar, parTuple2)
import           Control.Applicative hiding (empty)

import           Data.Proxy
import           Data.Semigroup
import           Data.Function (on)
import           Data.Hashable
import           Safe.Foldable (maximumByMay)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Set as S

import           Prelude as P hiding (reverse)

import Data.Ord.Graph (Graph, reverse, idxs, idxSet)
import Data.Ord.Graph.AlgorithmsGeneric (astar, pathCost)

type Heuristic i n = (i -> i -> n)

-- the very limited map interface we need to
-- provide generic instances for hashable and
-- ord types
class GMap map k v where
    insert :: k -> v -> map k v -> map k v
    get :: map k v -> k -> v
    fromList :: [(k, v)] -> map k v
    union :: map k v -> map k v -> map k v

instance (Hashable k, Eq k) => GMap HM.HashMap k v where
    insert = HM.insert
    get = (HM.!)
    fromList = HM.fromList
    union = HM.union

instance (Eq k, Ord k) => GMap M.Map k v where
    insert = M.insert
    get = (M.!)
    fromList = M.fromList
    union = M.union

-- nicer type synonyms for fixing the kind of map we get back
type HashCache i n = HM.HashMap ((i, i), (i, i)) n
type OrdCache i n = M.Map ((i, i), (i, i)) n

-- Given a set of predefined landmarks, and a lookup function between them and any
-- index in the graph, provide a heuristic function that uses the landmarks to provide
-- more intelligent routing.
heuristicWithLandmarks :: (Num n, Ord n) => [i] -> Heuristic i n -> Heuristic i n
heuristicWithLandmarks landmarks d v t = maximum [ bound | l <- landmarks
                                                         , bound <- [(d l t) - (d l v)
                                                                    ,(d v l) - (d t l)
                                                                    ]
                                                         ]

-- once you have your map, and your set of landmarks, you can get a heuristic function
-- You should only need to use this function if you are implementing a more efficient map
-- for your index type.
heuristicWithLandmarksMap :: (GMap map (i, i) n, Eq i, Num n, Ord n) => map (i, i) n -> [i] -> Heuristic i n
heuristicWithLandmarksMap m is = heuristicWithLandmarks is (\i j -> if i == j then 0 else m `get` (i, j))

-- If you know in advance which landmarks you want to use, you can generate a heuristic function
-- from them. However, since it is best for landmarks to be at the edge of the graph, you may
-- wish to use one of the seed builder functions instead, which will provide automatic landmark selection
--
-- In order to use this generic function, you need to fix the type of the intermediate distance cache; you
-- can do this with a proxy, or use one of @landmarkHeuristicOrd@ or @landmarkHeuristicHashable@.
landmarkHeuristic :: forall i n e v map. (GMap map (i, i) n, Ord i, Num n, Ord n) => Proxy (map (i, i) n)
                  -> [i] -> Graph i e v -> (e -> n) -> Heuristic i n -> Heuristic i n
landmarkHeuristic _ landmarks g measureEdge guessDistance = heuristicWithLandmarks landmarks d
    where
          d i j = distances `get` (i, j)

          -- calculate the distances in parallel
          distances :: map (i, i) n
          distances = fromList $ withStrategy (parList (parTuple2 rseq rpar)) (landmarks >>= toFrom)
          toFrom = distancesToAndFrom measureEdge guessDistance g (reverse g) (idxs g)

landmarkHeuristicHashable :: (Num n, Ord i, Eq i, Hashable i, Ord n)
                          => [i] -> Graph i e v -> (e -> n) -> Heuristic i n -> Heuristic i n
landmarkHeuristicHashable = landmarkHeuristic (Proxy :: Proxy (HM.HashMap (i, i) n))

landmarkHeuristicOrd :: (Num n, Ord i, Ord n)
                     => [i] -> Graph i e v -> (e -> n) -> Heuristic i n -> Heuristic i n
landmarkHeuristicOrd = landmarkHeuristic (Proxy :: Proxy (M.Map (i, i) n))

landmarkHeuristicFromSeedHashable :: forall i e v n. (Hashable i, Ord i, Num n, Ord n, Eq i)
                                  => Int -> i -> Graph i e v -> (e -> n) -> Heuristic i n -> Heuristic i n
landmarkHeuristicFromSeedHashable k seed g f h = let (ls, m) = selectLandmarks k seed g f h
                                                  in heuristicWithLandmarksMap (m :: HM.HashMap (i, i) n) ls

landmarkHeuristicFromSeedOrd :: forall i e v n. (Ord i, Num n, Ord n, Eq i)
                             => Int -> i -> Graph i e v -> (e -> n) -> Heuristic i n -> Heuristic i n
landmarkHeuristicFromSeedOrd k seed g f h = let (ls, m) = selectLandmarks k seed g f h
                                             in heuristicWithLandmarksMap (m :: M.Map (i, i) n) ls

-- a suitable k might be in the rang of 10 - 20 for a large graph
selectLandmarks :: forall map i e v n distances .
                (GMap map (i, i) n, Ord i, Num n, Ord n, distances ~ map (i, i) n)
                  => Int -> i -> Graph i e v -> (e -> n) -> Heuristic i n -> ([i], distances)
selectLandmarks k i g measureEdge guessDistance = findLandmarks [] (fromList []) k (Just i)
    where
        findLandmarks :: [i] -> distances -> Int -> (Maybe i) -> ([i], distances)
        findLandmarks ls m 0 _       = (ls, m) -- we have enough
        findLandmarks ls m _ Nothing = (ls, m) -- no starting point
        findLandmarks ls m required (Just pos) =
            let ls' = pos : ls -- this point is now a landmark
                is' = F.foldr S.delete is ls' -- no need to compute paths to the existing landmarks
                m' = m `union` fromList (sparkPairs $ toFrom m ls is' pos)
            in findLandmarks ls' m' (required - 1) (mostDistantFrom is' ls' m')

        -- if we already have enough landmarks, use them
        toFrom m ls = let h = (if length ls < 3 then guessDistance else heuristicWithLandmarksMap m ls)
                      in distancesToAndFrom measureEdge h g rg
        is = idxSet g
        rg = reverse g

        -- the whole point of precomputing landmarks is that we expect to need them all, and that
        -- there are probably a lot of nodes. Therefore, it is likely to be very helpful to evaluate
        -- the paths in parallel.
        sparkPairs = withStrategy (parList (parTuple2 rseq rpar))

-- ugly auxiliary functions

distancesToAndFrom :: (Eq i, Ord i, Num n, Ord n, Foldable f)
                   => (e -> n) -> Heuristic i n -> Graph i e v -> Graph i e v -> f i -> i -> [((i, i), n)]
distancesToAndFrom measure heuristic graph rgraph is i = F.concatMap (\j -> if i == j 
                                                                            then [((i,j), 0)]
                                                                            else f graph (i, j) ++ f rgraph (j, i)
                                                                     ) is
    where f g p = fmap ((,) p . pathCost measure) $ uncurry (astar heuristic measure g) p

mostDistantFrom :: (GMap map (i, i) n, Num n, Ord n, Foldable f) => f i -> [i] -> map (i, i) n -> Maybe i
mostDistantFrom candidates avoid m = fst <$> maximumByMay (compare `on` snd) distances
    where distances = F.concatMap (\c -> return (c, sumDist c)) candidates
          sumDist c = getSum $ foldMap (\a -> Sum (m `get` (c, a))) avoid
