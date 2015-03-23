{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Birth (birth) where

import Prelude ((==), (/=), (+), (.), Int, ($), fst, snd)
import qualified Prelude as L --hiding (head, id, foldr, foldr1, length, take, filter, map, zipWith, map, (++), or, and)

--import Data.Vector ((!), fromList, toList, Vector)
import Data.Vector

import Data.List.Split (chunksOf, splitPlaces)
--import qualified Data.List as L

import System.Random

import Stuff
import Change

--_and conditions x = and $ map ($ x) conditions
--_or conditions x = or $ map ($ x) conditions


birth :: Int -> RandomGenerator -> People -> People
birth iteration gen people = p ++ foldr (++) empty babies
	where
		p = accumulate (\p b -> p { children = (children p) ++ b }) people $ foldr f (fromList []) babies
			where
				f :: People -> Vector (ID, Vector ID) -> Vector (ID, Vector ID)
				f n list = list ++ fromList [(fst parrents', id'), (snd parrents', id')]
					where 
						parrents' = (parrents.head) n
						id' = map id n

		babies = zipWith f babyMakers $ vsplitPlaces numberOfBabies ids
			where
				f :: Person -> Vector Int -> People
				f mother ids = map (\id' -> makeBaby id' Male (id mother, lover mother)) ids

		babyMakers :: People
		babyMakers = filter (((==Female).gender) .&&. ((/=0).lover)) people 

		numberOfBabies :: Vector Int
		numberOfBabies = fromList $ L.take (length babyMakers) (randomRs (0, 5) gen :: [Int])

		ids = fromList [start..start + foldr1 (+) numberOfBabies]
		start = ((+1).id.head) people


makeBaby :: ID -> Gender -> (ID, ID) -> Person
makeBaby id gender parrents = Person id 0 gender None Endorphi 0 parrents empty [] (0,0)




--birth :: Int -> RandomGenerator -> People -> People
--birth iteration gen people = fromList $ p ++ b
--	where
--		(p, b) = f (toList people) (randomRs (0, 5) gen :: Int) $ ((+1).id.last) people



--f :: [Person] -> [Int] -> [[Int]] -> ([Person], [Person])
--f [] _ _ = ([],[])
--f (person:people) randomList id
--	| isBabyMaker person = let
--		next = f people (tail randomList) (id + (head randomList))
--		(female, children) = makeBabies person (head randomList) id
--		in (female : fst next, children ++ snd next)
--	| otherwise = let next = f people randomList id
--		in (person : fst next, snd next)
--	where
--		isBabyMaker person = gender person == Female && inRelation (relations person)

--makeBabies :: Person -> [Float] -> Int -> (Person, [Person])
--makeBabies female random id = foo female babies
--	where
----		babies = map fst $ filter f $ zip possibleBabies randomList
----			where f (_, p) = p > 0.5
--
----		possibleBabies = [Person i 0 g None [] | (i, g) <- zip ids (cycle [Female, Male])]
--		babies = [Person i 0 g None [] | (i, g) <- zip [id..] $ take random $ repeat $ cycle [Female, Male]]
--
--		foo :: Person -> [Person] -> (Person, [Person])
--		foo parrent [] = (parrent, [])
--		foo parrent children = (addRelations (parrent, parrentToChild), map addRelations $ zip children (zipWith (:) childToParrent siblingRelations))
--			where
--				parrentToChild = [Relation Child (id c) | c <- children]
--				childToParrent = [Relation Parrent (id parrent) | c <- children] ++ [Relation Child (
--
--				siblingRelations = [[Relation Sibling (id (children!!i)) | i' <- [0..length children], i /= i'] | i <- [0..length children]]
--
--		addRelations :: (Person, Relations) -> Person
--		addRelations (p, r) = Person (id p) (age p) (gender p) (proffesion p) (relations p ++ r)

