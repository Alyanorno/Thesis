{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float
import GHC.Conc (numCapabilities)

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
import Data.IORef
import System.IO.Unsafe
import System.Random
import System.Random.Mersenne.Pure64 as R
import System.Random.MWC
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Control.Monad.Parallel as P
import Stuff



change :: RandomGenerator -> [(People, Friends)] -> [(People, Friends)]
change gen peoples = map (\(people, friends) -> let ap = VB.filter alive people in ((relations ap friends).(home ap).(job ap)) people) peoples
	where
		relations :: People -> People -> Friends -> (People, Friends)
		relations alivePeople p friends = createRelations gen friends p alivePeople pb (professionalRelations pb) cb (culturalRelations cb)
			where cb = (cultureBuckets alivePeople); pb = (professionalBuckets alivePeople)

		job :: People -> People -> People
		job alivePeople p = VB.imap (\i a -> if age a == 20 then getAJob alivePeople chans a (V.slice (i*off) off random) else a) p
			where
 				(random,_) = randomVector_ (size*off) (pureMT $ fromIntegral $ fromXorshift gen)
				off = 2

				chans :: [Double]
 				chans = map float2Double [chansFarmer, chansAdministrator, chansBeggar, 0]
				chansFarmer = demand farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = (((fromIntegral size) / n) / ratio) / 2
							where n = fromIntegral $ VB.length $ VB.filter ((==work).profession) p
				chansAdministrator = f size
					where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
				chansBeggar = 1

				size = VB.length p

		home :: People -> People -> People
		home alivePeople p = VB.imap (\i a -> if age a == 20 then (let (safe, a') = safeAccess p (fst $ parrents a) in if not safe then p .! i else getAHome mapRange center maps a (position a') (Xorshift $ random ! i)) else a) p
			where
				random :: Vector Int64
				random = randomVector (VB.length p) (pureMT $ fromIntegral $ fromXorshift gen)

				center :: (Float, Float)
				center = let l = VB.filter (/=(0,0)) $ VB.map position $ alivePeople in (\(x,y)->((int2Float x)/(int2Float $ VB.length l), (int2Float y)/(int2Float $ VB.length l))) $ VB.foldr (\(x,y)(x',y')->(x+x',y+y')) (0,0) l

		maps :: VB.Vector (Vector Float)
		maps = VB.map perCulture allCulturesVector
			where
				perCulture :: Int -> Vector Float
				perCulture culture = concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture) .+. (culturalMap .! culture) .+. staticTerrainMap
					where (.+.) = V.zipWith (+)

				distanceFromCulturalCenter :: VB.Vector (Vector Float)
				distanceFromCulturalCenter = VB.map (V.map scaleDistanceFromCulturalCenter) $
					VB.map (\i -> let
						peoplePos = V.convert $ VB.map position $ cultureBuckets .! i
						average (x,y) = let l = V.length peoplePos in if l == 0 then (0,0) else ((int2Float x) / (int2Float $ V.length peoplePos), (int2Float y) / (int2Float $ V.length peoplePos))
						culturalCenter = average $ V.foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) peoplePos
						in V.map ((distanceTo $ culturalCenter).toFloat) positionMap)
						$ allCulturesVector

				concentrationOfPeopleMap :: Vector Float
				concentrationOfPeopleMap = V.convert $ VB.map (scaleConcentrationOfPeople.fromIntegral.VB.length) peopleMap

				culturalMap :: VB.Vector (Vector Float)
				culturalMap = VB.map (\i -> boxFilter $ V.convert $ VB.map (scaleCulturalMap.(f i)) peopleMap) $ allCulturesVector
					where 
						f i p = let m = VB.map (relationsTo i) p; l = (VB.length m) in if l == 0 then 0 else (VB.sum m) / int2Float l

						relationsTo :: Int -> Person -> Float
						relationsTo i p = (culturalRelations .! i) ! ((cultureToInt.culture) p)

				peopleMap :: VB.Vector People
				peopleMap = VB.accumulate VB.snoc (VB.replicate (V.length positionMap) VB.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) alivePeople
					where 
						f p = (((\(x,y) -> x + y * range).position) p, p)
--				peopleMap = toBuckets (VB.fromList [0..VB.length positionMap]) ((\(x,y) -> x + y * range).position) $ VB.filter ((/=(0,0)).position) alivePeople

				positionMap :: Vector (Int, Int)
				positionMap = V.fromList [(x,y) | x <- [0..range-1], y <- [0..range-1]]

		range = mapRange -- defined in Stuff.hs

		numberProfessions = length allProfessions
		numberCultures = length allCultures

		professionalBuckets :: People -> VB.Vector People
		professionalBuckets p = toBuckets allProfessionsVector (professionToInt.profession) p

		cultureBuckets :: People -> VB.Vector People
		cultureBuckets p = toBuckets allCulturesVector (cultureToInt.culture) p

		sampleSize = 100

		professionalRelations :: VB.Vector People -> VB.Vector (Vector Float)
		professionalRelations professionalBuckets = VB.zipWith (V.zipWith (+)) staticProfessionalRelations $ relationsBetween gen people numberProfessions (professionToInt.profession) professionalBuckets sampleSize

		culturalRelations :: VB.Vector People -> VB.Vector (Vector Float)
		culturalRelations cultureBuckets = relationsBetween gen people numberCultures (cultureToInt.culture) cultureBuckets sampleSize


createRelations :: RandomGenerator -> Friends -> People -> People -> VB.Vector People -> VB.Vector (Vector Float) -> VB.Vector People -> VB.Vector (Vector Float) -> (People, Friends)
--createRelations gen people alivePeople professionals professionalRelations culturals culturalRelations = VB.modify (\v -> applyMatches 0 v matches) people
createRelations gen friends people alivePeople professionals professionalRelations culturals culturalRelations = {-VB.modify (\v -> applyMatches v 0) people-} unsafePerformIO $ do
	let s = VB.length people
	let offset = numCapabilities
	v <- VB.unsafeThaw people
	fends <- VB.unsafeThaw friends
	vRef <- newMVar v

--	if s < 1000 then
--		applyMatches vRef fends $ zip (map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)) [(0, s)]
--	else
--		applyMatches vRef fends $ zip (map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)) $ let t = [0, s `div` offset..s] in zip t (tail t)

	applyMatches vRef fends $ zip (map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)) $ let t = [0, s `div` offset..s] in zip t (tail t)

	t <- takeMVar vRef
	v' <- VB.unsafeFreeze t
	fends' <- VB.unsafeFreeze fends
	return (v', fends')
	where
--	applyMatches :: MVar (MB.MVector RealWorld Person) -> Int -> (ID, Vector ID) -> IO ()
	applyMatches :: MVar (MB.MVector RealWorld Person) -> MB.MVector RealWorld (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyMatches vRef friends list = P.mapM (\(gen, (from, to)) -> applyMatches' from to gen) list
		where
		applyMatches' :: Int -> Int -> PureMT -> IO ()
		applyMatches' i max gen
			| i >= max = return ()
			| (not.alive) person = applyMatches' (i+1) max gen
--			| (((/=female).gender) .||. ((/=0).lover) .||. (not.alive) .||. ((>40).age)) person = next gen
			| (((==female).gender) .&&. ((==0).lover) .&&. ((<41).age)) person && fst m /= 0 = do
				let start = id $ VB.head people;
				i' <- (return $ (fst m) - start);

				withMVar vRef (\v -> do
					p' <- MB.read v i';
					p <- MB.read v i;
					when (lover p' == 0 && lover p == 0) $ do
						MB.write v i' (p' {lover = start + i});
						MB.write v i (p {lover = fst m});)

				friend <- MB.read friends i;
				if V.length friend + V.length (snd m) < 200 then do
					MB.write friends i (friend V.++ (snd m));
				else do
					MB.write friends i ((V.drop (V.length (snd m)) friend) V.++ (snd m));

				next
--			| V.null (snd m) = next
			| otherwise = do
				friend <- MB.read friends i;
				if V.length friend + V.length (snd m) < 200 then do
					MB.write friends i (friend V.++ (snd m));
				else do
					MB.write friends i ((V.drop (V.length (snd m)) friend) V.++ (snd m));
				next
			where
			next = applyMatches' (i+1) max gen'

			person = people .! i

--			gen = gens ! i

			m :: (ID, Vector ID)
--			m = head matches
--			m = match person $ pureMT $ fromIntegral $ gen
			(m, gen') = match (people .! i) gen

--	gens = (V.iterateN (VB.length people) (fromXorshift.step.Xorshift) (fromXorshift gen))

	match :: Person -> PureMT -> ((ID, Vector ID), PureMT)
	match person gen
		| V.null potentialMates = ((0, V.empty), gen)
		| otherwise = ((potentialMates ! r, V.map (potentialMates !) $ randomInts (V.length potentialMates-1) ((timeStep*5),0)), gen')
		where
			r :: Int
			r = V.head $ randomInts (V.length potentialMates-1) (1, 0)

			potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture

			{-# INLINE conditions #-}
			conditions x = (((/=gender person).gender) .&&. (((10<) .&&. (<50)).age) .&&. ((==0).lover) .&&. ((((/=) (parrents person)).parrents))) x

			randomNum = (timeStep*2, 0)
			sameProfNum = (timeStep*2, offset randomNum)
			sameCultNum = (timeStep*2, offset sameProfNum)
			profNum = (timeStep, offset sameCultNum)
			cultNum = (timeStep, offset profNum)
			profNumInternal = (sizeProfessionals, offset cultNum)
			cultNumInternal = (sizeCulturals, offset profNumInternal)

			offset (a,b) = a + b

			sizeProfessionals = V.sum $ profs
			sizeCulturals = V.sum $ cults

			potentialRandom = g alivePeople randomNum
			potentialSameProfessional = g prof sameProfNum
			potentialSameCulture = g cult sameCultNum

			{-# INLINE g #-}
			g v s = V.map (id.(v .!)) $ V.filter (conditions.(v .!)) $ randomInts (VB.length v-1) s


			potentialProfessional :: Vector ID
			potentialProfessional
				| V.null potential = V.empty
				| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts (V.length potential-1) profNum
				where
					potential :: Vector Int
					potential = f 0 V.empty $ snd profNumInternal
					f :: Int -> Vector Int -> Int -> Vector Int
					f i v offset
						| i >= VB.length professionals = v
						| VB.null p || profs ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector Int
							result = V.map (id.(p .!)) $ randomInts (VB.length p-1) ((profs ! i), offset)
							offset' = offset + profs ! i
							p = professionals .! i

			profs = V.map (floor.(* (2 * int2Float timeStep))) $ professionalRelations .! (professionToInt $ profession person)

			potentialCulture :: Vector Int
			potentialCulture
				| V.null potential = V.empty
				| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts (V.length potential-1) cultNum
				where
					potential :: Vector Int
					potential = f 0 V.empty $ snd cultNumInternal
					f :: Int -> Vector Int -> Int -> Vector Int
					f i v offset
						| i >= VB.length culturals = v
						| VB.null p || cults ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector Int
							result = V.map (id.(p .!)) $ randomInts (VB.length p-1) ((cults ! i), offset)
							offset' = offset + cults ! i
							p = culturals .! i

			cults = V.map (floor.(* (2 * int2Float timeStep))) $ culturalRelations .! (cultureToInt $ culture person)


--			randomInts :: Int -> Int -> PureMT -> VB.Vector Int
--			randomInts size max gen = VB.map (floor.(* (int2Float max)).double2Float) $ randomVector_ size gen

			randomInts :: Int -> (Int, Int) -> Vector Int
			randomInts max (size, from) = V.map (floor.(* (int2Float max)).double2Float) $ V.slice from size random

			random :: Vector Double
			(random, gen') = randomVector_ (offset cultNumInternal) gen

			prof :: VB.Vector Person
			prof = professionals .! (professionToInt $ profession person)

			cult :: VB.Vector Person
			cult = culturals .! (cultureToInt $ culture person)

getAJob :: People -> [Double] -> Person -> Vector Double -> Person
getAJob people chans person random
	| profession person == none = person {profession = f allProfessions random chans}
	| otherwise = person
	where
		i = (professionToInt.profession.(people .!).snd.parrents) person
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
--			| null possibleHomes || parrentPosition == (0,0) = person
			| parrentPosition == (0,0) = person
			| null possibleHomes = person {position = parrentPosition}
			| otherwise = person {position = home}
			where home = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
		map = maps .! (cultureToInt $ culture person)
		possibleHomes = filter ((/= infinity).valueAt)  [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
		range' = 3
		valueAt p@(x,y) = (map ! (x+y*range)) + (professionValue $ profession person) * (scaleDistanceFromCenter $ distanceTo center (toFloat p))


toBuckets :: VB.Vector Int -> (Person -> Int) -> People -> VB.Vector People
toBuckets buckets test list = VB.map (f list) buckets
	where f list i = VB.filter ((==i).test) list
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
		lovers list i = VB.length $ VB.filter ((==i).getGroup.(people &!).lover) $ VB.filter (((/=0).lover) .&&. (((id $ VB.head people)<).lover)) list

		statisticalSample :: Int -> People
		statisticalSample i = VB.map ((groupBuckets .! i) .!) $ VB.fromList $ take sampleSize (randomRs (0, VB.length groupBuckets - 1) gen :: [Int])



