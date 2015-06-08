{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Birth (birth) where


import Prelude hiding (id)

import Data.Vector.Unboxed ((!), toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (splitPlaces)

import System.Random

import Stuff
import Change



birth :: RandomGenerator -> (People, Friends) -> (People, Friends)
birth gen (people, friends) = (p VB.++ add, friends VB.++ VB.replicate (VB.length add) V.empty)
	where
		add = VB.fromList $ concat babies
		p = VB.accum (\p' b -> p' {children = (children p') V.++ (V.fromList $ b)}) people $ concat $ map f babies
			where
				f :: [Person] -> [(ID, [ID])]
				f person = [(p1-offset, b), (p2-offset, b)]
					where 
						(p1, p2) = (parrents.head) person
						offset = id $ VB.head people
						b = map id person

		babies :: [[Person]]
		babies = filter (not.null) $ zipWith f babyMakers $ splitPlaces numberOfBabies ids
			where
				f :: Person -> [Int] -> [Person]
				f mother ids'
					| null ids' = []
					| otherwise = map (\id' -> makeBaby id' (id mother, lover mother) (culture mother)) ids'

		babyMakers :: [Person]
--		babyMakers = VB.toList $ VB.filter (\a -> f (safeAccess p (lover a)) a) people
		babyMakers = VB.toList $ VB.filter conditions people
			where 
				f :: (Bool, Person) -> Person -> Bool
				f (safe, l) a
					| not safe || not (alive l) = False
					| otherwise = conditions a

				conditions :: Person -> Bool
				conditions a = (((==female).gender) .&&. ((/=0).lover) .&&. ((<45).age) .&&. alive) a

		numberOfBabies :: [Int]
		numberOfBabies = take (length babyMakers) $ (randomRs (0, timeStep `div` 2) gen :: [Int])

		ids = [start..start + foldr1 (+) numberOfBabies]
		start = ((+1).id.VB.last) people


makeBaby :: ID -> (ID, ID) -> Culture -> Person
makeBaby id' parrents' culture' = Person 0 (if mod id' 2 == 0 then male else female) none culture' 0 id' 0 parrents' V.empty (0,0)


