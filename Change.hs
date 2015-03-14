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
change gen professions cultures people = ((job.love.aged) people, professions, cultures)
	where
		aged people = V.map (\a -> a { age = age a + 10 }) people
		job people = (getAJob chansFarmer chansAdministrator) <$> people <*> (fromList $ take size $ randomListsOf_ 2 gen)
			where
				chansFarmer = demand Farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral (V.length people)) / n) / ratio) / 2
							where n = fromIntegral $ V.length $ V.filter ((==work).profession) people
				chansAdministrator = f (V.length people)
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
		love people = (createLovers people professionalBuckets cultureBuckets) <$> randomInts <*> randomFloats <*> people
			where
				randomInts = fromList $ take size $ chunksOf 10 (randomRs (0, size - 1) gen :: [Int])
				randomFloats = fromList $ take size $ randomListsOf_ 11 gen

		size = V.length people

		numberProfessions = (length [0..(fromEnum (maxBound :: Profession))])
		numberCultures = (length [0..(fromEnum (maxBound :: Culture))])

		professionalBuckets :: Vector People
		professionalBuckets = toBuckets numberProfessions profession people

		cultureBuckets :: Vector People
		cultureBuckets = toBuckets numberCultures culture people

		sampleSize = 100
		professionalRelations = relationsBetween gen people numberProfessions profession professionalBuckets sampleSize
		culturalRelations = relationsBetween gen people numberCultures culture cultureBuckets sampleSize



createLovers :: People -> Vector People -> Vector People -> [Int] -> [Float] -> Person -> Person
createLovers people professionals culturals randomInts randomFloats person
	| ((==0).lover) person = breakUp
	| otherwise = m
	where
		breakUp = if head randomFloats > 0.9 then m else person
		m = match person people randomInts (tail randomFloats)

		match :: Person -> People -> [Int] -> [Float] -> Person
		match person people randomInts randomFloats
			| length mates == 0 = person
			| otherwise = person { lover = id $ head mates }
			where
				mates = map fst $ filter (\(p, r) -> r > 0.7) $ zip potential randomFloats

				potential = potentialRandom ++ potentialProfessional
				potentialRandom = [p | i <- drop 5 randomInts, let p = people ! i, gender person /= gender p, ((/=0).lover) p]
				
				potentialProfessional = [p |
					i <- map (rescale (V.length people) (V.length prof)) $ take 5 randomInts,
					let p = prof ! i, gender person /= gender p, ((/=0).lover) p]

				prof :: Vector Person
				prof = professionals ! (fromEnum $ profession person)


getAJob :: Float -> Float -> Person -> [Float] -> Person
getAJob chansFarmer chansAdministrator person random
	| profession person == None = person { profession = f random }
	| otherwise = person
	where f random
		| chansFarmer > random !! 0 = Farmer
		| chansAdministrator > random !! 1 = Administrator
		| otherwise = Beggar


toBuckets :: (Enum e) => Int -> (Person -> e) -> People -> Vector People
toBuckets size test list = V.map (f list) $ fromList [0..size]
	where f list i = V.filter ((==i).fromEnum.test) list

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


