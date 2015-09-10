{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Birth (birth) where


import Prelude hiding (id)

import Data.Int (Int32)

import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (splitPlaces)

import System.Random

import Stuff
import Change



birth :: Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
birth gen (people, friends, childrens) = (people V.++ add, friends VB.++ VB.replicate (V.length add) V.empty, VB.empty) --c VB.++ VB.replicate (V.length add) V.empty)
	where
		add = V.fromList $ concat babies
{-		c = VB.accum (\a b -> a V.++ (V.fromList $ b)) childrens $ concat $ map f babies
			where
			f :: [Person] -> [(Int, [ID])]
			f person = [(fromID $ p1-offset, b), (fromID $ p2-offset, b)]
				where 
				(p1, p2) = (parrents.head) person
				offset = id $ V.head people
				b = map id person -}

		babies :: [[Person]]
		babies = filter (not.null) $ zipWith f babyMakers $ splitPlaces numberOfBabies ids
			where
			f :: Person -> [Int] -> [Person]
			f mother ids'
				| null ids' = []
				| otherwise = map (\id' -> makeBaby id' (id mother, lover mother) (culture mother)) ids'

		babyMakers :: [Person]
		babyMakers = V.toList $ V.filter conditions people
			where 
			conditions :: Person -> Bool
			conditions a = (((==female).gender) .&&. ((/=0).lover) .&&. ((<45).age) .&&. alive) a

		numberOfBabies :: [Int]
		numberOfBabies = take (length babyMakers) $ (randomRs (0, timeStep `div` 2) gen :: [Int])

		ids = [start..start + foldr1 (+) numberOfBabies]
		start = fromID $ ((+1).id.V.last) people


makeBaby :: Int -> (ID, ID) -> Culture -> Person
makeBaby id' parrents' culture' = Person 0 (if mod id' 2 == 0 then male else female) none culture' 0 (toID id') 0 parrents' (0,0)


