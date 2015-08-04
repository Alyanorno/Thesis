{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float
import GHC.Conc (numCapabilities)

import Data.Vector.Generic (convert)
import Data.Vector.Storable ((!), toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L
import Foreign.Storable.Tuple

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import Data.IORef
import System.IO.Unsafe
import System.Random
import System.Random.Mersenne.Pure64 as R
import System.Random.MWC
import Control.Exception (assert)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Control.Monad.Parallel as P
import Stuff



change :: RandomGenerator -> (People, Friends, Childrens) -> (People, Friends, Childrens)
change gen (people, friends, childrens) = let (p, f) = ((relations alivePeople friends).(home alivePeople).(job alivePeople)) people in (p, f, childrens)
	where
		alivePeople = V.filter alive people

		relations :: People -> Friends -> People -> (People, Friends)
		relations alivePeople friends p = createRelations gen friends p alivePeople professionalBuckets professionalRelations cultureBuckets culturalRelations

		job :: People -> People -> People
		job alivePeople p = V.imap (\i a -> if age a == 20 then getAJob alivePeople chans a (V.slice (i*off) off random) else a) p
			where
 				(random,_) = randomVector_ (size*off) (pureMT $ fromIntegral $ fromXorshift gen)
				off = 2

				chans :: [Double]
 				chans = map float2Double [chansFarmer, chansAdministrator, chansBeggar, 0]
				chansFarmer = demand farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral size) / n) / ratio) / 2
							where n = fromIntegral $ V.length $ V.filter ((==work).profession) p
				chansAdministrator = f size
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
				chansBeggar = 1

				size = V.length p

		home :: People -> People -> People
		home alivePeople p = V.imap (\i a -> if age a == 20 then (let (safe, a') = safeAccess p (fst $ parrents a) in if not safe then p ! i else getAHome mapRange center maps a (position a') (Xorshift $ random ! i)) else a) p
			where
				random :: Vector Int64
				random = randomVector (V.length p) (pureMT $ fromIntegral $ fromXorshift gen)

				center :: (Float, Float)
				center = let l = V.filter (/=(0,0)) $ V.map position $ alivePeople in (\(x,y)->((int2Float x)/(int2Float $ V.length l), (int2Float y)/(int2Float $ V.length l))) $ V.foldr (\(x,y)(x',y')->(x+x',y+y')) (0,0) l

		maps :: VB.Vector (Vector Float)
		maps = VB.map perCulture allCulturesVector
			where
				perCulture :: Int -> Vector Float
				perCulture culture = concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture) .+. (culturalMap .! culture) .+. staticTerrainMap
					where (.+.) = V.zipWith (+)

				distanceFromCulturalCenter :: VB.Vector (Vector Float)
				distanceFromCulturalCenter = VB.map (V.map scaleDistanceFromCulturalCenter) $
					VB.map (\i -> let
						peoplePos = V.map position $ cultureBuckets .! i
						average (x,y) = let l = V.length peoplePos in if l == 0 then (0,0) else ((int2Float x) / (int2Float $ V.length peoplePos), (int2Float y) / (int2Float $ V.length peoplePos))
						culturalCenter = average $ V.foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) peoplePos
						in V.map ((distanceTo $ culturalCenter).toFloat) positionMap)
						$ allCulturesVector

				concentrationOfPeopleMap :: Vector Float
				concentrationOfPeopleMap = V.convert $ VB.map (scaleConcentrationOfPeople.fromIntegral.V.length) peopleMap

				culturalMap :: VB.Vector (Vector Float)
				culturalMap = VB.map (\i -> boxFilter $ V.convert $ VB.map (scaleCulturalMap.(f i)) peopleMap) $ allCulturesVector
					where 
						f i p = let m = V.map (relationsTo i) p; l = (V.length m) in if l == 0 then 0 else (V.sum m) / int2Float l

						relationsTo :: Int -> Person -> Float
						relationsTo i p = (culturalRelations .! i) ! ((cultureToInt.culture) p)

				peopleMap :: VB.Vector People
				peopleMap = VB.accumulate V.snoc (VB.replicate (V.length positionMap) V.empty) $ VB.convert $ V.map f $ V.filter ((/=(0,0)).position) $ alivePeople
					where 
						f p = (((\(x,y) -> x + y * range).position) p, p)
--				peopleMap = toBuckets (VB.fromList [0..VB.length positionMap]) ((\(x,y) -> x + y * range).position) $ VB.filter ((/=(0,0)).position) alivePeople

				positionMap :: Vector (Int, Int)
				positionMap = V.fromList [(x,y) | x <- [0..range-1], y <- [0..range-1]]

		range = mapRange -- defined in Stuff.hs

		numberProfessions = length allProfessions
		numberCultures = length allCultures

		professionalBuckets :: VB.Vector People
		professionalBuckets = toBuckets allProfessionsVector (professionToInt.profession) alivePeople

		cultureBuckets :: VB.Vector People
		cultureBuckets = toBuckets allCulturesVector (cultureToInt.culture) alivePeople

		sampleSize = 100

		professionalRelations :: VB.Vector (Vector Float)
		professionalRelations = VB.zipWith (V.zipWith (+)) staticProfessionalRelations $ relationsBetween gen people numberProfessions (professionToInt.profession) professionalBuckets sampleSize

		culturalRelations :: VB.Vector (Vector Float)
		culturalRelations = relationsBetween gen people numberCultures (cultureToInt.culture) cultureBuckets sampleSize


createRelations :: RandomGenerator -> Friends -> People -> People -> VB.Vector People -> VB.Vector (Vector Float) -> VB.Vector People -> VB.Vector (Vector Float) -> (People, Friends)
createRelations gen friends people alivePeople professionals professionalRelations culturals culturalRelations = unsafePerformIO $ do
	let s = V.length people
	-- If fixed the result will not change depending on number of threads
	let offset = 12 --numCapabilities
	let randomGenerators = map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)
	let genWithRanges = zip randomGenerators $ let t = [0, s `div` offset..s-offset] ++ [s] in zip t (tail t)

	fends <- VB.unsafeThaw friends
	applyFriendMatches fends genWithRanges
	fends' <- VB.unsafeFreeze fends

	v <- V.unsafeThaw people
	applyLoveMatches v fends' genWithRanges
	v' <- V.unsafeFreeze v

	return (v', fends')
	where
	applyFriendMatches :: MB.MVector RealWorld (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyFriendMatches friends list = P.mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i max gen
			| i >= max = return ()
			| (not.alive) (people ! i) = applyMatches (i+1) max gen
			| otherwise = do
				friend <- MB.read friends i;
				if V.length friend + V.length m < 200 then do
					MB.write friends i (friend V.++ m);
				else do
					MB.write friends i ((V.drop (V.length m) friend) V.++ m);
				applyMatches (i+1) max gen'
			where
			m :: Vector ID
			(m, gen') = match (people ! i) gen

	applyLoveMatches :: M.MVector RealWorld Person -> VB.Vector (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyLoveMatches v fends list = mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i max gen
			| i >= max = return ()
			| (not.alive) person = next
			| V.null friends = next
			| ((((==female).gender) .&&. ((==0).lover) .&&. ((<41).age)) person) = do
				when ((not.V.null) potentialLovers) $ do
					i' <- (return $ m - start);

					p' <- M.read v i';
					p <- M.read v i;
					when (lover p' == 0) $ do
						M.write v i' (p' {lover = start + i});
						M.write v i (p {lover = m});
				next
			| otherwise = next
			where
			start = id $ V.head people
			next = applyMatches (i+1) max gen'
			person = people ! i
			friends = fends .! i

			potentialLovers :: Vector ID
			potentialLovers = V.filter (conditions.(people &!)) $ V.filter (\x -> x - start >= 0) friends

			{-# INLINE conditions #-}
			conditions x = (alive .&&. (((10<) .&&. (<50)).age) .&&. ((==0).lover) .&&. ((((/=) (parrents person)).parrents))) x

			m = potentialLovers ! r
			(r, gen') = randomR (0, V.length potentialLovers-1) gen


	match :: Person -> PureMT -> (Vector ID, PureMT)
	match person gen
		| V.null potentialMates = (V.empty, gen)
		| otherwise = (V.map (potentialMates !) $ randomInts (V.length potentialMates-1) ((timeStep*5),0), gen')
		where
			potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture

			randomNum = (timeStep*2, 0)
			sameProfNum = (timeStep*2, offset randomNum)
			sameCultNum = (timeStep*2, offset sameProfNum)
			profNum = (timeStep, offset sameCultNum)
			cultNum = (timeStep, offset profNum)
			profNumInternal = (timeStep*2, offset cultNum)
			cultNumInternal = (timeStep*2, offset profNumInternal)
--			nrRandomNumbers = 20
--			nrRandomNumbers = offset cultNumInternal
			nrRandomNumbers = offset cultNum

			offset (a,b) = a + b

			potentialRandom = g alivePeople randomNum
			potentialSameProfessional = g prof sameProfNum
			potentialSameCulture = g cult sameCultNum

			{-# INLINE g #-}
			g :: People -> (Int, Int) -> Vector ID
			g v s = V.map (id.(v !)) $ randomInts (V.length v-1) s


			potentialProfessional :: Vector ID
			potentialProfessional
				| V.null potential = V.empty
				| otherwise = V.map (potential !) $ randomInts (V.length potential-1) profNum
				where
					potential :: Vector Int
					potential = assert (V.sum profs <= fst profNumInternal) $ f 0 V.empty 0

					f :: Int -> Vector Int -> Int -> Vector Int
					f i v offset
						| i >= V.length profs = v
						| V.null p || profs ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector Int
							result = V.map (id.(p !)) $ randomInts (V.length p-1) ((profs ! i), offset)
							offset' = offset + profs ! i
							p = professionals .! i
			
			profs = V.map (floor.(* (int2Float $ fst profNumInternal))) $ normalize $ professionalRelations .! (professionToInt $ profession person)

			potentialCulture :: Vector Int
			potentialCulture
				| V.null potential = V.empty
				| otherwise = V.map (potential !) $ randomInts (V.length potential-1) cultNum
				where
					potential :: Vector Int
					potential = assert (V.sum cults <= fst cultNumInternal) $ f 0 V.empty 0

					f :: Int -> Vector Int -> Int -> Vector Int
					f i v offset
						| i >= V.length cults = v
						| V.null p || cults ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector Int
							result = V.map (id.(p !)) $ randomInts (V.length p-1) ((cults ! i), offset)
							offset' = offset + cults ! i
							p = culturals .! i

			cults = V.map (floor.(* (int2Float $ fst cultNumInternal))) $ normalize $ culturalRelations .! (cultureToInt $ culture person)

			normalize v = V.map (/ (V.sum v)) v

--			randomInts :: Int -> Int -> PureMT -> VB.Vector Int
--			randomInts size max gen = VB.map (floor.(* (int2Float max)).double2Float) $ randomVector_ size gen

			randomInts :: Int -> (Int, Int) -> Vector Int
			randomInts max (size, from) = V.map (floor.(* (int2Float max)).double2Float) $ V.slice from size random

			random :: Vector Double
			(random, gen') = randomVector_ nrRandomNumbers gen

			prof :: People
			prof = professionals .! (professionToInt $ profession person)

			cult :: People
			cult = culturals .! (cultureToInt $ culture person)


getAJob :: People -> [Double] -> Person -> Vector Double -> Person
getAJob people chans person random
	| profession person == none = person {profession = f allProfessions random chans}
	| otherwise = person
	where
	i = (professionToInt.profession.(people !).snd.parrents) person
	chans' = (take i chans) ++ ((chans' !! i) + 0.2) : (drop (i+1) chans)

	f prof random chans
		| chans !! 0 >= random ! 0 = prof !! 0
		| chans !! 1 >= random ! 1 = prof !! 1
		| otherwise = prof !! 2


getAHome :: Int -> (Float,Float) -> VB.Vector (Vector Float) -> Person -> (Int,Int) -> RandomGenerator -> Person
getAHome range center maps person parrentPosition gen
	| ((>10).age) .&&. ((/=0).lover) .&&. ((==(0,0)).position) $ person = theHome
	| otherwise = person
	where
	theHome
--		| null possibleHomes || parrentPosition == (0,0) = person
		| parrentPosition == (0,0) = person
		| null possibleHomes = person {position = parrentPosition}
		| otherwise = person {position = home}
		where home = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
	map = maps .! (cultureToInt $ culture person)
	possibleHomes = filter ((/= infinity).valueAt) [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
	range' = 3
	valueAt p@(x,y) = (map ! (x+y*range)) + (professionValue $ profession person) * (scaleDistanceFromCenter $ distanceTo center (toFloat p))


toBuckets :: VB.Vector Int -> (Person -> Int) -> People -> VB.Vector People
toBuckets buckets test list = VB.map (f list) buckets
	where f list i = V.filter ((==i).test) list
-- This should one day be faster, one day (snoc is O(n) for stupid haskell reasons)
--toBuckets buckets test list = VB.accumulate VB.snoc (VB.replicate (VB.length buckets) VB.empty) $ VB.zip (VB.map test list) list


-- Take a sample of x size from each group
-- Count number of lovers from that sample to each other group
-- Calculate percentage based number of lovers to each group
relationsBetween :: RandomGenerator -> People -> Int -> (Person -> Int) -> VB.Vector People -> Int -> VB.Vector (Vector Float)
relationsBetween gen people sizeGroup getGroup groupBuckets sampleSize = VB.map procentOfLovers numberOfLovers
	where
	procentOfLovers :: Vector Int -> Vector Float
	procentOfLovers list = V.map ((/ total).fromIntegral) list
		where total = fromIntegral $ V.foldr1 (+) list

	numberOfLovers :: VB.Vector (Vector Int)
	numberOfLovers = VB.fromList [V.fromList [lovers s j | j <- list] | i <- list, let s = statisticalSample i]
		where list = [0..sizeGroup-1]

	lovers :: People -> Int -> Int
	lovers list i = V.length $ V.filter ((==i).getGroup.(people &!).lover) $ V.filter (((/=0).lover) .&&. (((id $ V.head people)<).lover)) list

	statisticalSample :: Int -> People
	statisticalSample i = V.map ((groupBuckets .! i) !) $ V.fromList $ take sampleSize (randomRs (0, VB.length groupBuckets - 1) gen :: [Int])



