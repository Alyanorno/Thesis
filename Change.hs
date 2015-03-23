{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Change (change, toBuckets) where

import Prelude hiding (id)

import Data.Vector ((!), fromList, toList, Vector)
import qualified Data.Vector as V

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import Control.Monad.ST
import Stuff




change :: RandomGenerator -> Professions -> Cultures -> People -> (People, Professions, Cultures)
change gen professions cultures people = ((home.job.love.aged) people, professions, cultures)
	where
		aged people = V.map (\a -> a { age = age a + 10 }) people
		love people = (createLovers people professionalBuckets professionalRelations cultureBuckets culturalRelations) <$> randomInts <*> randomFloats <*> people
			where
				randomInts = fromList $ take size $ chunksOf 15 (randomRs (0, size - 1) gen :: [Int])
				randomFloats = fromList $ take size $ randomListsOf_ 12 gen
		job people = (getAJob chansFarmer chansAdministrator) <$> people <*> (fromList $ take size $ randomListsOf_ 2 gen)
			where
				chansFarmer = demand Farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral (V.length people)) / n) / ratio) / 2
							where n = fromIntegral $ V.length $ V.filter ((==work).profession) people
				chansAdministrator = f (V.length people)
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1

		home :: People -> People
		home people = (getAHome maps) <$> people <*> (fromList $ take size (randomRs (0, 1) gen :: [Float]))

		maps = let (.+.) = V.zipWith (+) in fromList [(distanceFromCulturalCenter ! culture) .+. distanceFromCenter .+. concentrationOfPeopleMap .+. (culturalMap ! culture) | culture <- [0..(fromEnum (maxBound :: Culture))]]
			where
				distanceFromCulturalCenter :: Vector (Vector Float)
				distanceFromCulturalCenter = fromList [ V.map (fromIntegral.(\(x,y) -> (x-cx)^2 + (y-cy)^2)) positionMap | i <- [0..(fromEnum (maxBound :: Culture))], let culturalCenter = V.map position $ cultureBuckets ! i, let (cx, cy) = (\(x,y) -> (div x (V.length culturalCenter), div y (V.length culturalCenter)))$ V.foldr1 (\(x,y) (x',y') -> (x+x',y+y')) culturalCenter]

				distanceFromCenter :: Vector Float
				distanceFromCenter = V.map (fromIntegral.(\(x,y) -> x^2 + y^2)) positionMap

				concentrationOfPeopleMap :: Vector Float
				concentrationOfPeopleMap = V.map ((2^).(*(-1)).fromIntegral.V.length) peopleMap

				culturalMap :: Vector (Vector Float)
				culturalMap = fromList [V.map (\p -> V.sum $ V.map (relationsTo i) p) peopleMap | i <- [0..(fromEnum (maxBound :: Culture))]]
					where relationsTo i p = culturalRelations ! i ! (fromIntegral.fromEnum.culture) p

				peopleMap :: Vector People
--				peopleMap = toBuckets positionMap position $ V.filter (((>10).age) .&&. ((==0).lover) .&&. ((/=(0,0)).position)) people
				peopleMap = V.accumulate V.snoc (V.replicate (V.length positionMap) V.empty) $ V.map f $ V.filter (((>10).age) .&&. ((==0).lover) .&&. ((/=(0,0)).position)) people
					where f p = (((\(x,y) -> x + y * range).position) p, p)

				positionMap :: Vector (Int, Int)
				positionMap = fromList [(x,y) | x <- [-range..range], y <- [-range..range]]
				range = 50

		size = V.length people

		numberProfessions = (length [0..(fromEnum (maxBound :: Profession))])
		numberCultures = (length [0..(fromEnum (maxBound :: Culture))])

		professionalBuckets :: Vector People
--		professionalBuckets = toBuckets numberProfessions profession people
		professionalBuckets = toBuckets (fromList [0..fromEnum (maxBound :: Profession)]) (fromEnum.profession) people

		cultureBuckets :: Vector People
--		cultureBuckets = toBuckets numberCultures culture people
		cultureBuckets = toBuckets (fromList [0..fromEnum (maxBound :: Culture)]) (fromEnum.culture) people

		sampleSize = 100
		professionalRelations = relationsBetween gen people numberProfessions profession professionalBuckets sampleSize
		culturalRelations = relationsBetween gen people numberCultures culture cultureBuckets sampleSize

-- TODO: Add people map (more people -> lower desire to live there)
-- TODO: Add cultural map (depending on relation between cultures, more or less desire to live there)
-- TODO: Distance from center (0,0)
-- TODO: Distance form cultural center (to be calculated)





createLovers :: People -> Vector People -> Vector (Vector Float) -> Vector People -> Vector (Vector Float) -> [Int] -> [Float] -> Person -> Person
createLovers people professionals professionalRelations culturals culturalRelations randomInts randomFloats person
	| ((==0).lover) person = breakUp
	| otherwise = m
	where
		breakUp = if head randomFloats > 0.9 then m else person
		m = match person people randomInts (tail randomFloats)

		match :: Person -> People -> [Int] -> [Float] -> Person
		match person people randomInts randomFloats
			| V.length potentialMates == 0 = person
			| otherwise = person { lover = id $ potentialMates ! round ((head randomFloats) * (fromIntegral $ (V.length potentialMates))) }
			where
				potentialMates :: People
				potentialMates = potentialRandom V.++ potentialProfessional V.++ potentialSameProfessional V.++ potentialCulture V.++ potentialSameCulture

				potentialRandom :: People
				potentialRandom = fromList [p | i <- take 5 $ drop 10 randomInts, let p = people ! i, gender person /= gender p, ((/=0).lover) p]

				potentialProfessional :: People
				potentialProfessional = V.foldr (V.++) V.empty $ V.map (\(prof,r) -> V.map ((prof !).(rescale (V.length people) (V.length prof))) r ) $ V.zip professionals $ vsplitPlaces profs $ fromList randomInts -- Change to drop a certain number of them and increase randomInts size
					where profs = V.map (floor.(*10)) $ professionalRelations ! (fromEnum $ profession person)

				potentialCulture :: People
				potentialCulture = V.foldr (V.++) V.empty $ V.map (\(cult,r) -> V.map ((cult !).(rescale (V.length people) (V.length cult))) r ) $ V.zip culturals $ vsplitPlaces cults $ fromList randomInts -- Change to drop a certain number of them and increase randomInts size
					where cults = V.map (floor.(*10)) $ culturalRelations ! (fromEnum $ culture person)

				potentialSameProfessional :: People
				potentialSameProfessional = fromList [p |
					i <- takeRandom prof (take 5),
					let p = prof ! i, gender person /= gender p, ((/=0).lover) p]

				potentialSameCulture :: People
				potentialSameCulture = fromList [p |
					i <- takeRandom prof ((take 5).(drop 5)),
					let p = cult ! i, gender person /= gender p, ((/=0).lover) p]

				prof :: Vector Person
				prof = professionals ! (fromEnum $ profession person)

				cult :: Vector Person
				cult = culturals ! (fromEnum $ culture person)

				takeRandom bucket f = map (rescale (V.length people) (V.length bucket)) $ f randomInts

getAJob :: Float -> Float -> Person -> [Float] -> Person
getAJob chansFarmer chansAdministrator person random
	| profession person == None = person { profession = f random }
	| otherwise = person
	where f random
		| chansFarmer > random !! 0 = Farmer
		| chansAdministrator > random !! 1 = Administrator
		| otherwise = Beggar

-- TOOD: Not done
getAHome :: Vector (Vector Float) -> Person -> Float -> Person
getAHome maps person random
	| ((>10).age) .&&. ((/=0).lover) .&&. ((==(0,0)).position) $ person = person { position = findHome (fromEnum $ culture person)}
	| otherwise = person
		where findHome culture = (0,0) --maps ! culture ! 0 -- TODO: Not Done



toBuckets :: Vector Int -> (Person -> Int) -> People -> Vector People
toBuckets buckets test list = V.map (f list) buckets
	where f list i = V.filter ((==i).fromEnum.test) list
-- This should one day be faster, one day (snoc is O(n) for stupid haskell reasons)
--toBuckets buckets test list = V.accumulate V.snoc (V.replicate (V.length buckets) V.empty) $ V.zip (V.map (test) list) list


-- Take a sample of x size from each group
-- Count number of lovers from that sample to each other group
-- Calculate percentage based number of lovers to each group
relationsBetween :: (Enum e, Eq e) => RandomGenerator -> People -> Int -> (Person -> e) -> Vector People -> Int -> Vector (Vector Float)
relationsBetween gen people sizeGroup getGroup groupBuckets sampleSize = V.map procentOfLovers numberOfLovers
	where
		procentOfLovers :: Vector Int -> Vector Float
		procentOfLovers list = V.map ((/ total).fromIntegral) list
			where total = fromIntegral $ V.foldr1 (+) list

		numberOfLovers :: Vector (Vector Int)
		numberOfLovers = fromList [fromList [lovers s j | j <- list] | i <- list, let s = statisticalSample i]
			where list = [0..sizeGroup-1]

		lovers :: People -> Int -> Int
		lovers list i = V.length $ V.filter ((==toEnum i).getGroup.(people !).lover) $ V.filter ((/=0).lover) list

		statisticalSample :: Int -> People
		statisticalSample i = V.map ((groupBuckets ! i) !) $ fromList $ take sampleSize (randomRs (0, V.length groupBuckets - 1) gen :: [Int])




-- Discontinued for now!
--magic = 10000.0 :: Float
--oldCultures, splitingCultures = partition (\((a, i), r) -> ((fromIntegral $ (length $ cultureBuckets ! i)) / magic) * (groupDivision a) > r) zip (zip cultures (fromList [0..]) (randomRs (0, 1) gen :: [Float])
--newCulture = map splitCulture splitingCultures


