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
--import Control.DeepSeq
--import Control.Concurrent
import Control.Concurrent.MVar
--import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Primitive
import Control.Concurrent.Async
import Control.Concurrent.ParallelIO
--import Control.Monad.Par
import Control.Monad.ST
import qualified Control.Monad.Parallel as P
--import Control.Monad.Loop.ForEach
--import Control.Parallel
--import Control.Parallel.Strategies
import Stuff



change :: RandomGenerator -> People -> (People, Vector Float)
change gen people' = ((home.job.relations.aged) people', maps .! 0)
	where
		aged :: People -> People
		aged p = VB.map (\a -> a { age = age a + timeStep }) p

		relations :: People -> People
		relations p = createRelations gen p (VB.filter alive p) professionalBuckets professionalRelations cultureBuckets culturalRelations

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

		people = VB.filter alive people'

		maps :: VB.Vector (Vector Float)
		maps = VB.map perCulture $ VB.fromList [0..(fromEnum (maxBound :: Culture))]
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
						$ VB.fromList [0..(fromEnum (maxBound :: Culture))]

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


createRelations :: RandomGenerator -> People -> People -> VB.Vector People -> VB.Vector (Vector Float) -> VB.Vector People -> VB.Vector (Vector Float) -> People
--createRelations gen people alivePeople professionals professionalRelations culturals culturalRelations = VB.modify (\v -> applyMatches 0 v matches) people
createRelations gen people alivePeople professionals professionalRelations culturals culturalRelations = {-VB.modify (\v -> applyMatches v 0) people-} unsafePerformIO $ do
	let s = VB.length people
	let offset = numCapabilities
	v <- VB.unsafeThaw people
--	vRef <- newMVar v

--	if s > 1000 then do
		--t1 <- async $ applyMatches v (offset*0) (offset*1-1)
		--t2 <- async $ applyMatches v (offset*1) (s-1)
		--wait t1
		--wait t2
-- 		P.mapM (\(from,to) -> applyMatches v from to) $ let t = [0, s `div` offset..s] in zip t (tail t)
	applyMatches v $ let t = [0, s `div` offset..s] in zip t (tail t)
--	else
		--mapM (\i' -> forM i' (\i -> applyMatches vRef i)) $ chunksOf (s `div` offset) [0..s-1]
		--applyMatches v 0 (s-1)
--		P.mapM (\(from,to) -> applyMatches v from to) $ [(0, s-1)]

--	mapM (\i -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [0..s-1]
--	P.mapM (\i' -> forM i' (\i -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i))) $ chunksOf chunkSize [0..s-1]
--	P.mapM (\indexs -> mapM (\(i,m) -> applyMatches vRef i m) $ zip indexs $ map (\i -> match (people .! i) (pureMT $ fromIntegral $ gens ! i)) indexs) $ chunksOf chunkSize [0..s-1]
--	P.mapM (\index -> V.imap (\i m -> applyMatches v (index + i) m) $ V.generate index (\i -> match (people .! (index + i)) (pureMT $ fromIntegral $ gens ! (index + i)))) $ [0,chunkSize..s-1] ++ [rem (s-1) chunkSize]

{-	t1 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*0..offset*1-1]
	t2 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*1..s-1]
	wait t1
	wait t2 -}
{-	t1 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*0..offset*1-1]
	t2 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*1..offset*2-1]
	t3 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*2..offset*3-1]
	t4 <- async $ mapM (\(i,m) -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) $ map (\i -> (i, match (people .! i) (pureMT $ fromIntegral $ gens ! i))) [offset*3..s-1]
	wait t1
	wait t2
	wait t3
	wait t4 -}

{-	t1 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [0..s-1])
	wait t1 -}
{-	t1 <- async (mapM (\i -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*0..offset*1-1])
	t2 <- async (mapM (\i -> applyMatches vRef i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*1..s-1])
	wait t1
	wait t2 -}
{-	t1 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*0..offset*1-1])
	t2 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*1..offset*2-1])
	t3 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*2..offset*3-1])
	t4 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*3..s-1])
	wait t1
	wait t2
	wait t3
	wait t4 -}
{-	t1 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*0..offset*1-1])
	t2 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*1..offset*2-1])
	t3 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*2..offset*3-1])
	t4 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*3..offset*4-1])
	t5 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*4..offset*5-1])
	t6 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*5..offset*6-1])
	t7 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*6..offset*7-1])
	t8 <- async (mapM (\i -> applyMatches v i $ match (people .! i) (pureMT $ fromIntegral $ gens ! i)) [offset*7..s-1])
	wait t1
	wait t2
	wait t3
	wait t4
	wait t5
	wait t6
	wait t7
	wait t8 -}

--	t <- takeMVar vRef
	v' <- VB.unsafeFreeze v
	return (v')
	where
--	applyMatches :: MVar (MB.MVector RealWorld Person) -> Int -> (ID, Vector ID) -> IO ()
	applyMatches :: MB.MVector RealWorld Person -> [(Int, Int)] -> IO [()]
	applyMatches !v !list = P.mapM (\(from,to) -> applyMatches' from to) list
--	applyMatches !v !list = parallel_ [applyMatches' from to | (from,to) <- list] -- stopGlobalPool
		where
		applyMatches' :: Int -> Int -> IO ()
		applyMatches' !i !max
			| i >= max = return ()
			| (not.alive) person = next
--			| (((/=Female).gender) .||. ((/=0).lover) .||. (not.alive) .||. ((>40).age)) person = next gen
			| (((==Female).gender) .&&. ((==0).lover) .&&. ((<41).age)) person && fst m /= 0 = do
--				withMVar vRef (\v -> do
				let start = id $ VB.head people;
				i' <- (return $ (fst m) - start);

				p' <- MB.read v i';
				MB.write v i' (p' {lover = start + i});

				p <- MB.read v i;
				if V.length (friends p) + V.length (snd m) < 200 then do
					MB.write v i (p {lover = fst m, friends = (friends p) V.++ (snd m)});
				else do
					MB.write v i (p {lover = fst m, friends = (V.drop (V.length (snd m)) (friends p)) V.++ (snd m)});
				next
--			| V.null (snd m) = next
			| otherwise = do
--				withMVar vRef (\v -> do
				p <- MB.read v i;
				if V.length (friends p) + V.length (snd m) < 200 then do
					MB.write v i (p {friends = (friends p) V.++ (snd m)});
				else do
					MB.write v i (p {friends = (V.drop (V.length (snd m)) (friends p)) V.++ (snd m)});
				next
			where
			next = applyMatches' (i+1) max

			person = people .! i

			gen = gens ! i

			m :: (ID, Vector ID)
--			m = head matches
--			m = match person $ pureMT $ fromIntegral $ gen
			m = match (people .! i) (pureMT $ fromIntegral $ gens ! i)

	gens = (V.iterateN (VB.length people) (fromXorshift.step.Xorshift) (fromXorshift gen))

	match :: Person -> PureMT -> (ID, Vector ID)
	match !person !gen
--		| VB.length people == 0 = 0
		| V.null potentialMates = (0, V.empty)
		| otherwise = (potentialMates ! r, V.map (potentialMates !) $ randomInts (timeStep*5) (V.length potentialMates-1) gen)
		where
			r :: Int
			(r,_) = randomR (0, V.length potentialMates-1) gen

			potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture

			{-# INLINE conditions #-}
			conditions !x = (((/=gender person).gender) .&&. ((<50).age) .&&. ((==0).lover) .&&. ((((/=) (parrents person)).parrents))) x

			randomNum = (timeStep*2, 0)
			sameProfNum = (timeStep*2, offset randomNum)
			sameCultNum = (timeStep*2, offset sameProfNum)
			profNum = (timeStep, offset sameCultNum)
			cultNum = (timeStep, offset profNum)
			profNumInternal = (sizeProfessionals, offset cultNum)
			cultNumInternal = (sizeCulturals, offset profNumInternal)

			offset (!a,!b) = a + b

			sizeProfessionals = V.sum $ profs
			sizeCulturals = V.sum $ cults

			potentialRandom = g alivePeople randomNum
			potentialSameProfessional = g prof sameProfNum
			potentialSameCulture = g cult sameCultNum

			{-# INLINE g #-}
			g !v !s = V.map (id.(v .!)) $ V.filter (conditions.(v .!)) $ randomInts_ (VB.length v-1) s


			potentialProfessional :: Vector ID
			potentialProfessional
				| V.null potential = V.empty
				| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts_ (V.length potential-1) profNum
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
							result = V.map (id.(p .!)) $ randomInts_ (VB.length p-1) ((profs ! i), offset)
							offset' = offset + profs ! i
							p = professionals .! i

			profs = V.map (floor.(* (2 * int2Float timeStep))) $ professionalRelations .! (fromEnum $ profession person)

			potentialCulture :: Vector Int
			potentialCulture
				| V.null potential = V.empty
				| otherwise = V.filter (conditions.(people &!)) $ V.map (potential !) $ randomInts_ (V.length potential-1) cultNum
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
							result = V.map (id.(p .!)) $ randomInts_ (VB.length p-1) ((cults ! i), offset)
							offset' = offset + cults ! i
							p = culturals .! i

			cults = V.map (floor.(* (2 * int2Float timeStep))) $ culturalRelations .! (fromEnum $ culture person)


			randomInts :: Int -> Int -> PureMT -> V.Vector Int
			randomInts size max gen = V.map (floor.(* (int2Float max)).double2Float) $ randomVector_ size gen

			randomInts_ :: Int -> (Int, Int) -> V.Vector Int
			randomInts_ max (size, from) = V.map (floor.(* (int2Float max)).double2Float) $ V.slice from size random

			random = randomVector_ (offset cultNumInternal) gen

			prof :: VB.Vector Person
			prof = professionals .! (fromEnum $ profession person)

			cult :: VB.Vector Person
			cult = culturals .! (fromEnum $ culture person)

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
--			| null possibleHomes || parrentPosition == (0,0) = person
			| parrentPosition == (0,0) = person
			| null possibleHomes = person {position = parrentPosition}
			| otherwise = person {position = home}
			where home = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
		map = maps .! (fromEnum $ culture person)
		possibleHomes = filter ((/= infinity).valueAt)  [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
		range' = 3
		valueAt p@(x,y) = (map ! (x+y*range)) + (professionValue $ profession person) * (scaleDistanceFromCenter $ distanceTo (toFloat (div range 2, div range 2)) (toFloat p))

--V.map (scaleDistanceFromCenter.().toFloat) positionMap


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



