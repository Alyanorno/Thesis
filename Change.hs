{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Generic (convert)
import Data.Vector.Unboxed ((!), toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad (liftM2)
import Control.Monad.ST
import Stuff




change :: RandomGenerator -> People -> (People, Vector Float)
change gen people' = ((home.job.love.aged) people', maps .! 0)
	where
		aged :: People -> People
		aged p = VB.map (\a -> a { age = age a + timeStep }) p

		love :: People -> People
		love p = createLovers gen p (VB.filter alive p) professionalBuckets professionalRelations cultureBuckets culturalRelations

		job :: People -> People
		job p = VB.imap (\i a -> getAJob people chans a (V.slice (i*off) off random)) p
			where
				random = randomVector_ (size*off) (pureMT $ fromIntegral $ fromXorshift gen)
				off = 2

				chans :: [Double]
 				chans = map float2Double [chansFarmer, chansAdministrator, chansBeggar, 0]
				chansFarmer = demand Farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral size) / n) / ratio) / 2
							where n = fromIntegral $ VB.length $ VB.filter ((==work).profession) p
				chansAdministrator = f size
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
				chansBeggar = 1

				size = VB.length p

		home :: People -> People
		home p = VB.imap (\i a -> let (safe, a') = safeAccess p (fst $ parrents a) in if not safe then p .! i else getAHome range maps a (position a') (Xorshift $ random ! i)) p
			where
				random :: Vector Int64
				random = randomVector (VB.length p) (pureMT $ fromIntegral $ fromXorshift gen) 

--				parrentsPositions :: VB.Vector (Int, Int)
--				parrentsPositions = (VB.map g p)
--					where g a = (position.(p &!).fst.parrents) a

		people = VB.filter alive people'

		maps :: VB.Vector (Vector Float)
		maps = VB.map perCulture $ VB.fromList [0..(fromEnum (maxBound :: Culture))]
			where
				perCulture :: Int -> Vector Float
				perCulture culture = distanceFromCenter .+. concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture) .+. (culturalMap .! culture) .+. staticTerrainMap
--				perCulture culture = culturalMap .! culture
					where (.+.) = V.zipWith (+)

--				distanceFromCulturalCenter :: Vector (Vector Float)
--				distanceFromCulturalCenter = V.map (V.map scaleDistanceFromCulturalCenter) $ V.map (\i -> let culturalCenter = V.map position $ cultureBuckets ! i; c = (\(x,y) -> (div x (V.length culturalCenter), div y (V.length culturalCenter))) $ V.foldr1 (\(x,y) (x',y') -> (x+x',y+y')) culturalCenter in V.map ((distanceTo $ toFloat c).toFloat) positionMap) $ fromList [0..(fromEnum (maxBound :: Culture))]
				distanceFromCulturalCenter :: VB.Vector (Vector Float)
				distanceFromCulturalCenter = VB.map (V.map scaleDistanceFromCulturalCenter) $
					VB.map (\i -> let
						peoplePos = V.convert $ VB.map position $ cultureBuckets .! i
						average (x,y) = let l = V.length peoplePos in if l == 0 then (0,0) else ((int2Float x) / (int2Float $ V.length peoplePos), (int2Float y) / (int2Float $ V.length peoplePos))
						culturalCenter = average $ V.foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) peoplePos
						in V.map ((distanceTo $ culturalCenter).toFloat) positionMap)
						$ VB.fromList [0..(fromEnum (maxBound :: Culture))]

				distanceFromCenter :: Vector Float
				distanceFromCenter = V.map (scaleDistanceFromCenter.(distanceTo $ toFloat (div range 2, div range 2)).toFloat) positionMap
				
				toFloat :: (Int,Int) -> (Float,Float)
				toFloat (x,y) = (fromIntegral x, fromIntegral y)

				distanceTo :: (Float,Float) -> (Float,Float) -> Float
				distanceTo (x,y) (x',y') = (x-x')^2 + (y-y')^2

				concentrationOfPeopleMap :: Vector Float
				concentrationOfPeopleMap = V.convert $ VB.map (scaleConcentrationOfPeople.fromIntegral.VB.length) peopleMap

				culturalMap :: VB.Vector (Vector Float)
				culturalMap = VB.map (\i -> boxFilter $ VB.convert $ VB.map (scaleCulturalMap.(f i)) peopleMap) $ VB.fromList [0..(fromEnum (maxBound :: Culture))]
					where 
						f i p = let m = VB.map (relationsTo i) p; l = (VB.length m) in if l == 0 then 0 else (VB.sum m) / int2Float l

						relationsTo :: Int -> Person -> Float
						relationsTo i p = (culturalRelations .! i) ! ((fromIntegral.fromEnum.culture) p)

				peopleMap :: VB.Vector People
				peopleMap = VB.accumulate VB.snoc (VB.replicate (V.length positionMap) VB.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) people
					where 
						f p = (((\(x,y) -> x + y * range).position) p, p)
--						conditions = ((>10).age) .&&. ((==0).lover) .&&. ((/=(0,0)).position)

				positionMap :: Vector (Int, Int)
				positionMap = V.fromList [(x,y) | x <- [0..range-1], y <- [0..range-1]]
		range = mapRange -- defined in Stuff.hs

		numberProfessions = (length [0..(fromEnum (maxBound :: Profession))])
		numberCultures = (length [0..(fromEnum (maxBound :: Culture))])

		professionalBuckets :: VB.Vector People
		professionalBuckets = toBuckets (VB.fromList [0..fromEnum (maxBound :: Profession)]) (fromEnum.profession) people

		cultureBuckets :: VB.Vector People
		cultureBuckets = toBuckets (VB.fromList [0..fromEnum (maxBound :: Culture)]) (fromEnum.culture) people

		sampleSize = 100

		professionalRelations :: VB.Vector (Vector Float)
		professionalRelations = VB.zipWith (V.zipWith (+)) staticProfessionalRelations $ relationsBetween gen people' numberProfessions profession professionalBuckets sampleSize

		culturalRelations :: VB.Vector (Vector Float)
		culturalRelations = relationsBetween gen people' numberCultures culture cultureBuckets sampleSize


createLovers :: RandomGenerator -> People -> People -> VB.Vector People -> VB.Vector (Vector Float) -> VB.Vector People -> VB.Vector (Vector Float) -> People
createLovers gen people alivePeople professionals professionalRelations culturals culturalRelations = VB.modify (\v -> loop 0 v gen) people
	where loop i v gen
		| i >= VB.length people = return ()
		| (((/=Female).gender) .||. ((/=0).lover) .||. (not.alive) .||. ((>40).age)) person = loop (i+1) v gen
		| m == 0 = loop (i+1) v gen
		| otherwise = do
			let start = id $ VB.head people;
			i' <- (return $ m - start);

			p' <- MB.read v i';
			MB.write v i' (p' {lover = start + i});

			p <- MB.read v i;
			MB.write v i (p {lover = m});

			loop (i+1) v gen'
		where
		person = people .! i

		(_,gen') = next gen


		m :: Int
		-- m = match person people (f 0) (f 1) (f 2) (f 3) (f 4) (f 5)
		m = match person people (f 0) (f 1) (f 2) (f 3) (f 4) (f 5) -- $ pureMT $ fromIntegral $ fromXorshift gen

		f :: Int -> PureMT
		f i = pureMT $ fromIntegral $ fromXorshift $ g i
			where
				g 0 = gen
				g i = let (_,n) = next $ g (i-1) in n

		match :: Person -> People -> PureMT -> PureMT -> PureMT -> PureMT -> PureMT -> PureMT -> ID
		match person people gen randGen sameProfGen sameCultGen profGen cultGen
			| VB.length people == 0 = 0
			| V.null potentialMates = 0
			| otherwise = potentialMates ! r
			where
				r :: Int
				(r,_) = randomR (0, V.length potentialMates-1) gen

				potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture
				conditions !x = (((/=gender person).gender) .&&. ((<50).age) .&&. ((==0).lover) .&&. ((((/=) (parrents person)).parrents))) x

				potentialRandom = g alivePeople (timeStep*2) randGen
				potentialSameProfessional = g prof (timeStep*2) sameProfGen
				potentialSameCulture = g cult (timeStep*2) sameCultGen

				g v s g = V.map (id.(v .!)) $ V.filter (conditions.(v .!)) $ randomInts s (VB.length v-1) g


				potentialProfessional :: Vector ID
				potentialProfessional
					| V.null potential = V.empty
					| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts timeStep (V.length potential-1) profGen
					where
						potential :: Vector Int
						potential = convert $ VB.foldr1 (VB.++) $ VB.imap f $ VB.fromList $ take (fromEnum (maxBound::Profession)) $ map fromXorshift $ iterate step $ Xorshift $ fst $ R.randomInt64 gen
						f :: Int -> Int64 -> VB.Vector Int
						f i g
							| VB.null p = VB.empty
							| otherwise = VB.map (id.(p .!)) $ convert $ randomInts (profs ! i) (VB.length p-1) $ pureMT $ fromIntegral g
							where p = professionals .! i

						profs = V.map (floor.(*10)) $ professionalRelations .! (fromEnum $ profession person)

				potentialCulture :: Vector Int
				potentialCulture
					| V.null potential = V.empty
					| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts timeStep (V.length potential-1) gen
					where
						potential :: Vector Int
						potential = convert $ VB.foldr1 (VB.++) $ VB.imap f $ VB.fromList $ take (fromEnum (maxBound::Culture)) $ map fromXorshift $ iterate step $ Xorshift $ fst $ R.randomInt64 gen
						f :: Int -> Int64 -> VB.Vector Int
						f i g
							| VB.null p = VB.empty
							| otherwise = VB.map (id.(p .!)) $ convert $ randomInts (cults ! i) (VB.length p-1) $ pureMT $ fromIntegral g
							where p = culturals .! i

						cults = V.map (floor.(*10)) $ culturalRelations .! (fromEnum $ culture person)


				randomInts :: Int -> Int -> PureMT -> V.Vector Int
				randomInts size max gen = V.map (floor.(* (int2Float max)).double2Float) $ randomVector_ size gen

				prof :: VB.Vector Person
				prof = professionals .! (fromEnum $ profession person)

				cult :: VB.Vector Person
				cult = culturals .! (fromEnum $ culture person)

--getAJob :: (A.IArray a Float, A.Ix i, Num i) => People -> [Float] -> Person -> a i e -> Person
getAJob :: People -> [Double] -> Person -> Vector Double -> Person
getAJob people chans person random
	| profession person == None = person {profession = f [minBound::Profession .. maxBound::Profession] random chans}
	| otherwise = person
	where
		i = (fromEnum.profession.(people .!).snd.parrents) person
		chans' = (take i chans) ++ ((chans' !! i) + 0.2) : (drop (i+1) chans)

		f prof random chans
			| chans !! 0 >= random ! 0 = prof !! 0
			| chans !! 1 >= random ! 1 = prof !! 1
			| otherwise = prof !! 2
--			| length prof < 3 = (head prof)
--			| (head chans) >= (head random) = (head prof) 
--			| otherwise = f (tail prof) (tail random) (tail chans)


getAHome :: Int -> VB.Vector (Vector Float) -> Person -> (Int,Int) -> RandomGenerator -> Person
getAHome range maps person parrentPosition gen
	| ((>10).age) .&&. ((/=0).lover) .&&. ((==(0,0)).position) $ person = theHome
	| otherwise = person
	where
		theHome
			| null possibleHomes || parrentPosition == (0,0) = person
			| otherwise = person {position = home}
			where home = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
		map = maps .! (fromEnum $ culture person)
		possibleHomes = filter ((/= infinity).valueAt)  [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
		range' = 3
		valueAt (x,y) = map ! (x+y*range)



toBuckets :: VB.Vector Int -> (Person -> Int) -> People -> VB.Vector People
toBuckets buckets test list = VB.map (f list) buckets
	where f list i = VB.filter ((==i).fromEnum.test) list
-- This should one day be faster, one day (snoc is O(n) for stupid haskell reasons)
--toBuckets buckets test list = V.accumulate V.snoc (V.replicate (V.length buckets) V.empty) $ V.zip (V.map (test) list) list


-- Take a sample of x size from each group
-- Count number of lovers from that sample to each other group
-- Calculate percentage based number of lovers to each group
relationsBetween :: (Enum e, Eq e) => RandomGenerator -> People -> Int -> (Person -> e) -> VB.Vector People -> Int -> VB.Vector (Vector Float)
relationsBetween gen people sizeGroup getGroup groupBuckets sampleSize = VB.map procentOfLovers numberOfLovers
	where
		procentOfLovers :: Vector Int -> Vector Float
		procentOfLovers list = V.map ((/ total).fromIntegral) list
			where total = fromIntegral $ V.foldr1 (+) list

		numberOfLovers :: VB.Vector (Vector Int)
		numberOfLovers = VB.fromList [V.fromList [lovers s j | j <- list] | i <- list, let s = statisticalSample i]
			where list = [0..sizeGroup-1]

		lovers :: People -> Int -> Int
		lovers list i = VB.length $ VB.filter ((==toEnum i).getGroup.(people &!).lover) $ VB.filter ((/=0).lover) list

		statisticalSample :: Int -> People
		statisticalSample i = VB.map ((groupBuckets .! i) .!) $ VB.fromList $ take sampleSize (randomRs (0, VB.length groupBuckets - 1) gen :: [Int])



