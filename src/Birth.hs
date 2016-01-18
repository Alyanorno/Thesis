module Birth (birth) where


import Prelude hiding (id)

import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import Data.List.Split (splitPlaces)

import System.Random

import Stuff hiding (start)
import Definitions



birth :: Options -> Xorshift -> (People, Friends) -> (People, Friends)
birth opt gen (people, friends) = (people V.++ add, friends VB.++ VB.replicate (V.length add) V.empty)
	where
		add = V.fromList . concat . babies $ ids

		babies :: [Int] -> [[Person]]
		babies = filter (not.null) . zipWith f babyMakers . splitPlaces numberOfBabies
			where
			f :: Person -> [Int] -> [Person]
			f mother ids'
				| null ids' = []
				| otherwise = map (\id' -> makeBaby id' (id mother, lover mother) (culture mother)) ids'

		babyMakers :: [Person]
		babyMakers = V.toList $ V.filter conditions people
			where 
			conditions :: Person -> Bool
			conditions = ((==female).gender) .&&. ((/=0).lover) .&&. ((<45).age) .&&. alive

		numberOfBabies :: [Int]
		numberOfBabies = take (length babyMakers) (randomRs (0, timeStep opt `div` 2) gen :: [Int])

		ids = [start..start + sum numberOfBabies]
		start = fromID $ ((+1).id.V.last) people


makeBaby :: Int -> (ID, ID) -> Culture -> Person
makeBaby id' parrents' culture' = Person 0 (if mod id' 2 == 0 then male else female) none culture' 0 (toID id') 0 parrents' (0,0)


