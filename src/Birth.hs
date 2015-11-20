{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Birth (birth) where


import Prelude hiding (id)

import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import Data.List.Split (splitPlaces)

import System.Random

import Definitions
import Stuff


birth :: Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
birth gen (people, friends, _) = (people V.++ add, friends VB.++ VB.replicate (V.length add) V.empty, VB.empty)
	where
		add = V.fromList $ concat babies

		babies :: [[Person]]
		babies = filter (not.null) $ zipWith f babyMakers $ splitPlaces numberOfBabies ids
			where
			f :: Person -> [Int] -> [Person]
			f mother ids'
				| null ids' = []
				| otherwise = map (\id' -> makeBaby id' (getId mother, lover mother) (culture mother)) ids'

		babyMakers :: [Person]
		babyMakers = V.toList $ V.filter conditions people
			where 
			conditions :: Person -> Bool
			conditions a = (((==female).gender) .&&. ((/=0).lover) .&&. ((<45).age) .&&. alive) a

		numberOfBabies :: [Int]
		numberOfBabies = take (length babyMakers) $ (randomRs (0, timeStep `div` 2) gen :: [Int])

		ids = [start..start + foldr1 (+) numberOfBabies]
		start = fromID $ ((+1).getId.V.last) people

makeBaby :: Int -> (ID, ID) -> Culture -> Person
makeBaby id' parrents' culture' = Person 0 (if mod id' 2 == 0 then male else female) none culture' 0 (toID id') 0 parrents' (0,0)
