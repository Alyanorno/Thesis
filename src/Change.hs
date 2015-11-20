{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float

--import Data.Vector.Strategies (parVector)
import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import qualified Data.List as L

import Foreign.Storable.Tuple ()

import Data.Int (Int32, Int64)
import System.Random hiding (next, random, randoms)
import System.Random.Mersenne.Pure64 as R
--import Control.Concurrent.MVar

import Relations
import Definitions
import Stuff



change :: Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
change gen (people, friends, childrens) = let (p, f) = relations . home . job $ people in (p, f, childrens)
	where
	alivePeople = V.filter alive people

	relations :: People -> (People, Friends)
	relations p = createRelations gen friends p alivePeople professionalBuckets professionalRelations cultureBuckets culturalRelations

	job :: People -> People
	job p = V.imap (\i a -> if age a == 20 then getAJob alivePeople chans a (V.unsafeSlice (i*off) off random) else a) p
		where
 		(random,_) = randomVector_ (size*off) (pureMT $ fromIntegral $ fromXorshift gen)
		off = 2

		chans :: [Float]
 		chans = map getChans allProfessions

		getChans :: Profession -> Float
		getChans prof
			| prof == farmer = demand farmer 3
			| prof == administrator = f size
			| prof == beggar = 1
			| otherwise = 0
			where
			f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
			demand :: Profession -> Float -> Float
			demand work ratio = (((fromIntegral size) / n) / ratio) / 2
				where n = fromIntegral $ V.length $ V.filter ((==work).profession) p

		size = V.length p

	home :: People -> People
	home p = V.imap (\i a -> if age a == 20 then maybe (p ! i) (\a' -> getAHome mapRange center maps a (position a') (Xorshift $ random ! i)) (safeAccess p ((fromID.fst.parrents) a)) else a) p
		where
		random :: Vector Int64
		random = randomVector (V.length p) (pureMT $ fromIntegral $ fromXorshift gen)

		center :: (Float, Float)
		center = let l = V.filter (/=(0,0)) $ V.map position $ alivePeople in (\(x,y)->((fromIntegral x)/(int2Float $ V.length l), (fromIntegral y)/(int2Float $ V.length l))) $ V.foldr (\(x,y)(x',y')->(x+x',y+y')) (0,0) l

	maps :: VB.Vector (Vector Float)
	maps = VB.map perCulture allCulturesVector
		where
		perCulture :: Int -> Vector Float
		perCulture culture' = concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture') .+. (culturalMap .! culture') .+. staticTerrainMap
			where (.+.) = V.zipWith (+)

		distanceFromCulturalCenter :: VB.Vector (Vector Float)
		distanceFromCulturalCenter = VB.map (V.map scaleDistanceFromCulturalCenter) $
			VB.map (\i -> let
				peoplePos = V.map (position . (people &!)) $ cultureBuckets .! i
				average :: (Int32,Int32) -> (Float,Float); average (x,y) = let l = V.length peoplePos in if l == 0 then (0,0) else ((fromIntegral x) / (int2Float $ V.length peoplePos), (fromIntegral y) / (int2Float $ V.length peoplePos))
				culturalCenter = average $ V.foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) peoplePos
				in V.map (distanceTo culturalCenter . toFloat) positionMap)
				$ allCulturesVector

		concentrationOfPeopleMap :: Vector Float
		concentrationOfPeopleMap = V.generate (VB.length peopleMap) (\i -> (scaleConcentrationOfPeople.fromIntegral.V.length) (peopleMap .! i))

		culturalMap :: VB.Vector (Vector Float)
		culturalMap = VB.map (\i -> boxFilter $ V.generate (VB.length peopleMap) (\l -> (scaleCulturalMap.(f i)) (peopleMap .! l))) $ allCulturesVector
			where 
			f i p = let m = V.map (relationsTo i) p; l = (V.length m) in if l == 0 then 0 else (V.sum m) / int2Float l

			relationsTo :: Int -> ID -> Float
			relationsTo i p = (culturalRelations .! i) ! (cultureToInt . culture . (people &!) $ p)

		peopleMap :: VB.Vector (Vector ID)
		peopleMap = VB.unsafeAccumulate V.snoc (VB.replicate (V.length positionMap) V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert alivePeople
			where f p = (fromIntegral . (\(x,y) -> x + y * range) . position $ p, getId p)

		positionMap :: Vector (Int32, Int32)
		positionMap = V.fromList [(x,y) | x <- [0..range-1], y <- [0..range-1]]

	range = mapRange -- defined in Stuff.hs

	numberProfessions = length allProfessions
	numberCultures = length allCultures

	professionalBuckets :: VB.Vector (Vector ID)
	professionalBuckets = toBuckets allProfessionsVector (professionToInt.profession) alivePeople

	cultureBuckets :: VB.Vector (Vector ID)
	cultureBuckets = toBuckets allCulturesVector (cultureToInt.culture) alivePeople

	sampleSize = 100

	professionalRelations :: VB.Vector (Vector Float)
	professionalRelations = VB.zipWith (V.zipWith (+)) staticProfessionalRelations $ relationsBetween gen people numberProfessions (professionToInt . profession) professionalBuckets sampleSize

	culturalRelations :: VB.Vector (Vector Float)
	culturalRelations = relationsBetween gen people numberCultures (cultureToInt . culture) cultureBuckets sampleSize


getAJob :: People -> [Float] -> Person -> Vector Float -> Person
getAJob people chans person random
	| profession person == none = person {profession = f allProfessions}
	| otherwise = person
	where
	i = professionToInt . profession . (people !) . (fromID :: ID -> Int) . snd . parrents $ person
	chans' = (take i chans) ++ ((chans' !! i) + 0.2) : (drop (i+1) chans)

	f prof
		| chans' !! 0 >= random ! (0 :: Int) = prof !! 0
		| chans' !! 1 >= random ! (1 :: Int) = prof !! 1
		| otherwise = prof !! 2


getAHome :: Int32 -> (Float,Float) -> VB.Vector (Vector Float) -> Person -> (Int32,Int32) -> Xorshift -> Person
getAHome range center maps person parrentPosition gen
	| (>10) . age .&&. (/=0) . lover .&&. (==(0,0)) . position $ person = theHome
	| otherwise = person
	where
	theHome
--		| null possibleHomes || parrentPosition == (0,0) = person
		| parrentPosition == (0,0) = person {position = (mapRange `div` 2, mapRange `div` 2)}
		| null possibleHomes = person {position = parrentPosition}
		| otherwise = person {position = futureHome}
		where futureHome = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
	mapy = maps .! (cultureToInt $ culture person)
	possibleHomes = filter ((/= infinity) . valueAt) [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
	range' = 3
	valueAt p@(x,y) = (mapy ! (x+y*range)) + (professionValue $ profession person) * (scaleDistanceFromCenter $ distanceTo center (toFloat p))


-- Take a sample of x size from each group
-- Count number of lovers from that sample to each other group
-- Calculate percentage based number of lovers to each group
relationsBetween :: Xorshift -> People -> Int -> (Person -> Int) -> VB.Vector (Vector ID) -> Int -> VB.Vector (Vector Float)
relationsBetween gen people sizeGroup getGroup groupBuckets sampleSize = VB.map procentOfLovers numberOfLovers
	where
	procentOfLovers :: Vector Int -> Vector Float
	procentOfLovers list = V.map ((/ total).fromIntegral) list
		where total = fromIntegral $ V.foldr1 (+) list

	numberOfLovers :: VB.Vector (Vector Int)
	numberOfLovers = VB.generate sizeGroup (\i -> let s = statisticalSample i in V.generate sizeGroup (\j -> lovers s j))

	lovers :: Vector ID -> Int -> Int
	lovers list i = V.length $ V.filter ((==i) . getGroup . (people &!) . lover) $ V.filter ((/=0) . lover .&&. ((getId . V.head $ people)<) . lover) $ V.map (people &!) list

	statisticalSample :: Int -> Vector ID
	statisticalSample i = V.map ((groupBuckets .! i) !) $ V.fromList $ take sampleSize (randomRs (0, VB.length groupBuckets - 1) gen :: [Int])

