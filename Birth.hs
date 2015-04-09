{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Birth (birth) where

--import Prelude ((==), (/=), (+), (.), Int, ($), fst, snd)
--import qualified Prelude as L --hiding (head, id, foldr, foldr1, length, take, filter, map, zipWith, map, (++), or, and)

import Prelude hiding (id)

import Data.Vector ((!), fromList, toList, Vector, empty)
import qualified Data.Vector as V

import Data.List.Split (chunksOf, splitPlaces)
--import qualified Data.List as L

import System.Random

import Stuff
import Change

--_and conditions x = and $ map ($ x) conditions
--_or conditions x = or $ map ($ x) conditions


birth :: Int -> RandomGenerator -> People -> People
birth iteration gen people = p V.++ (fromList $ concat babies)
	where
--		p = V.accum (\p b -> p {children = (children p) V.++ (fromList $ b)}) people $ foldr f [] babies
--			where
--				f :: [Person] -> [(ID, [ID])] -> [(ID, [ID])]
--				f n list = list ++ [(p1, id'), (p2, id')]
--					where 
--						(p1, p2) = (parrents.head) n
--						id' = map id n

		p = V.accum (\p b -> p {children = (children p) V.++ (fromList $ b)}) people $ concat $ map f babies
			where
				f :: [Person] -> [(ID, [ID])]
				f p = [(p1-offset, b), (p2-offset, b)]
					where 
						(p1, p2) = (parrents.head) p
						offset = id $ people ! 0
						b = map id p

		babies :: [[Person]]
		babies = filter (not.null) $ zipWith f babyMakers $ splitPlaces numberOfBabies ids
			where
				f :: Person -> [Int] -> [Person]
				f mother ids
					| null ids = []
					| otherwise = map (\id' -> makeBaby id' (id mother, lover mother) (culture mother)) ids

		babyMakers :: [Person]
		babyMakers = toList $ V.filter (((==Female).gender) .&&. ((/=0).lover) .&&. ((<41).age) .&&. ((/=(0,0)).position) .&&. alive) people 

		numberOfBabies :: [Int]
		numberOfBabies = take (length babyMakers) $ (randomRs (0, timeStep `div` 2) gen :: [Int])

		ids = [start..start + foldr1 (+) numberOfBabies]
		start = ((+1).id.V.last) people


makeBaby :: ID -> (ID, ID) -> Culture -> Person
makeBaby id parrents culture = Person True id 0 (toEnum $ mod id 2) None culture 0 parrents empty [] (0,0)


