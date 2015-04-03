{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Change (change, toBuckets) where

import Prelude hiding (id)

import Data.Vector ((!), fromList, toList, Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import Control.Monad (liftM2)
import Control.Monad.ST
import Stuff




change :: RandomGenerator -> Professions -> Cultures -> People -> People
change gen professions cultures people' = (home.job.love.aged) people'
	where
		aged p = V.map (\a -> a { age = age a + 10 }) p

		love p = createLovers gen p professionalBuckets professionalRelations cultureBuckets culturalRelations
		job p = (getAJob chansFarmer chansAdministrator) <$> p <*> (randomVectorsOf 2 size gen (0, 1) :: Vector [Float])
			where
				chansFarmer = demand Farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral (V.length p)) / n) / ratio) / 2
							where n = fromIntegral $ V.length $ V.filter ((==work).profession) p
				chansAdministrator = f (V.length p)
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1

		home :: People -> People
		home p = (getAHome range maps) <$> p <*> (V.map g p) <*> (V.generate size f)
			where
				g = (position.(\x -> p ! (x - id (p ! 0))).fst.parrents)

				f :: Int -> StdGen
				f 0 = mkStdGen $ fromIntegral $ fromXorshift gen
				f i = n
					where (_,n) = next $ f (i-1)

		size = V.length people';
		people = V.filter alive people'

		maps = V.map perCulture $ fromList [0..(fromEnum (maxBound :: Culture))]
			where
				perCulture culture = (distanceFromCulturalCenter ! culture) .+. distanceFromCenter .+. concentrationOfPeopleMap .+. (culturalMap ! culture)
					where (.+.) = V.zipWith (+)

				distanceFromCulturalCenter :: Vector (Vector Float)
				distanceFromCulturalCenter = V.map (\i -> let culturalCenter = V.map position $ cultureBuckets ! i; (cx, cy) = (\(x,y) -> (div x (V.length culturalCenter), div y (V.length culturalCenter))) $ V.foldr1 (\(x,y) (x',y') -> (x+x',y+y')) culturalCenter in V.map (fromIntegral.(\(x,y) -> (x-cx)^2 + (y-cy)^2)) positionMap) $ fromList [0..(fromEnum (maxBound :: Culture))]

				distanceFromCenter :: Vector Float
				distanceFromCenter = V.map (fromIntegral.(\(x,y) -> x^2 + y^2)) positionMap

				concentrationOfPeopleMap :: Vector Float
				concentrationOfPeopleMap = V.map ((2^).(*(-1)).fromIntegral.V.length) peopleMap

				culturalMap :: Vector (Vector Float)
				culturalMap = V.map (\i -> V.map (\p -> V.sum $ V.map (relationsTo i) p) peopleMap) $ fromList [0..(fromEnum (maxBound :: Culture))]
					where relationsTo i p = culturalRelations ! i ! (fromIntegral.fromEnum.culture) p

				peopleMap :: Vector People
				peopleMap = V.accumulate V.snoc (V.replicate (V.length positionMap) V.empty) $ V.map f $ V.filter conditions people
					where 
						f p = (((\(x,y) -> x + y * range).position) p, p)
						conditions = ((>10).age) .&&. ((==0).lover) .&&. ((/=(0,0)).position)

				positionMap :: Vector (Int, Int)
				positionMap = fromList [(x,y) | x <- [-range..range], y <- [-range..range]]
		range = 50

		numberProfessions = (length [0..(fromEnum (maxBound :: Profession))])
		numberCultures = (length [0..(fromEnum (maxBound :: Culture))])

		professionalBuckets :: Vector People
		professionalBuckets = toBuckets (fromList [0..fromEnum (maxBound :: Profession)]) (fromEnum.profession) people

		cultureBuckets :: Vector People
		cultureBuckets = toBuckets (fromList [0..fromEnum (maxBound :: Culture)]) (fromEnum.culture) people

		sampleSize = 100
		professionalRelations = relationsBetween gen people numberProfessions profession professionalBuckets sampleSize
		culturalRelations = relationsBetween gen people numberCultures culture cultureBuckets sampleSize


createLovers :: RandomGenerator -> People -> Vector People -> Vector (Vector Float) -> Vector People -> Vector (Vector Float) -> People
createLovers gen people professionals professionalRelations culturals culturalRelations = V.modify (\v -> loop 0 v gen) people
	where loop i v gen
		| i >= V.length people = return ()
		| (((/=Female).gender) .||. ((/=0).lover) .||. (not.alive)) person = loop (i+1) v gen
		| m == 0 = loop (i+1) v gen
		| otherwise = do
			let start = id $ people ! 0;
			i' <- (return $ m - start);

			p' <- M.read v i';
			M.write v i' (p' {lover = start + i});

			p <- M.read v i;
			M.write v i (p {lover = m});

			loop (i+1) v gen'
		where
		person = people ! i

		(_,gen') = next gen

		m :: Int
		m = match person people (f 0) (f 1) (f 2) (f 3) (f 4) (f 5)

		f :: Int -> StdGen
		f 0 = mkStdGen $ fromIntegral $ fromXorshift gen
		f i = n
			where (_,n) = next $ f (i-1)

		match :: Person -> People -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> StdGen -> ID
		match person people gen randomGen profGen cultGen sameProfGen sameCultGen
			| null potentialMates = 0
			| otherwise = id $ potentialMates !! r
			where
				r :: Int
				(r,_) = randomR (0, (length potentialMates)-1) gen

				potentialMates = filter (((/=gender person).gender) .&&. ((==0).lover)) $ potentialRandom ++ potentialProfessional ++ potentialSameProfessional ++ potentialCulture ++ potentialSameCulture


				potentialRandom = map (people !) $ take 5 $ (randomRs (0, V.length people-1) randomGen)
				potentialSameProfessional = map (prof !) $ take 5 $ randomRs (0, V.length prof-1) sameProfGen
				potentialSameCulture = map (cult !) $ take 5 $ randomRs (0, V.length cult-1) sameCultGen



--				potentialProfessional :: People
--				potentialProfessional = V.foldr (V.++) V.empty $
--					V.map (\(prof,r) ->
--						V.map ((prof !).(rescale (V.length people) (V.length prof))) r ) $
--							V.zip professionals $ vsplitPlaces profs $ fromList randomInts

				potentialProfessional :: [Person]
				potentialProfessional = concat $ map f $ zip [0..(fromEnum (maxBound :: Profession))] $ iterate (snd.next) profGen
					where
						f (i,g) = [p ! n | n <- take (profs ! i) (randomRs (0, V.length p-1) g)]
							where p = professionals ! i

						profs = V.map (floor.(*10)) $ professionalRelations ! (fromEnum $ profession person)

--				potentialCulture :: People
--				potentialCulture = V.foldr (V.++) V.empty $ V.map (\(cult,r) -> V.map ((cult !).(rescale (V.length people) (V.length cult))) r ) $ V.zip culturals $ vsplitPlaces cults $ fromList positionrandomInts
--					where cults = V.map (floor.(*10)) $ culturalRelations ! (fromEnum $ culture person)

				potentialCulture :: [Person]
				potentialCulture = concat $ map f $ zip [0..(fromEnum (maxBound :: Culture))] $ iterate (snd.next) cultGen
					where
						f (i,g) = [p ! n | n <- take (cults ! i) (randomRs (0, V.length p-1) g)]
							where p = culturals ! i

						cults = V.map (floor.(*10)) $ culturalRelations ! (fromEnum $ culture person)



				prof :: Vector Person
				prof = professionals ! (fromEnum $ profession person)

				cult :: Vector Person
				cult = culturals ! (fromEnum $ culture person)


getAJob :: Float -> Float -> Person -> [Float] -> Person
getAJob chansFarmer chansAdministrator person random
	| profession person == None = person { profession = f random }
	| otherwise = person
	where f random
		| chansFarmer > random !! 0 = Farmer
		| chansAdministrator > random !! 1 = Administrator
		| otherwise = Beggar


getAHome :: Int -> Vector (Vector Float) -> Person -> (Int,Int) -> StdGen -> Person
getAHome range maps person parrentPosition gen
	| ((>10).age) .&&. ((/=0).lover) .&&. ((==(0,0)).position) $ person = person --{ position = theHome}
	| otherwise = person
		where
			theHome = L.maximumBy (\(x,y) (x',y') -> compare (map ! (x+y*range*2)) (map ! (x'+y'*range*2))) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes) gen)]
			map = maps ! (fromEnum $ culture person)
			possibleHomes = [(x,y) | let (x',y') = parrentPosition, (x,y) <- zip [x'-range'..x'+range'] [y'-range'..y'+range'], x < max, x > 0, y < max * 2, y > 0]
			range' = 2
			max = (range * 2) ^ 2



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
		lovers list i = V.length $ V.filter ((==toEnum i).getGroup.(people .!).lover) $ V.filter ((/=0).lover) list

		statisticalSample :: Int -> People
		statisticalSample i = V.map ((groupBuckets ! i) !) $ fromList $ take sampleSize (randomRs (0, V.length groupBuckets - 1) gen :: [Int])




-- Discontinued for now!
--magic = 10000.0 :: Float
--oldCultures, splitingCultures = partition (\((a, i), r) -> ((fromIntegral $ (length $ cultureBuckets ! i)) / magic) * (groupDivision a) > r) zip (zip cultures (fromList [0..]) (randomRs (0, 1) gen :: [Float])
--newCulture = map splitCulture splitingCultures


