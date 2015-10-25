{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float
import GHC.Conc (numCapabilities)

import Data.Vector.Strategies (parVector)
import Data.Vector.Generic (convert)
import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L
import Foreign.Storable.Tuple

import Data.Int (Int32, Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import Data.IORef
import System.IO.Unsafe
import System.Random
import System.Random.Mersenne.Pure64 as R
import System.Random.MWC
import Control.Parallel.Strategies
import Control.Exception (assert)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Control.Monad.Parallel as P
import Stuff



change :: Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
change gen (people, friends, childrens) = let (p, f) = relations alivePeople friends . home alivePeople . job alivePeople $ people in (p, f, childrens)
	where
		alivePeople = V.filter alive people

		relations :: People -> Friends -> People -> (People, Friends)
		relations alivePeople friends p = createRelations gen friends p alivePeople professionalBuckets professionalRelations cultureBuckets culturalRelations

		job :: People -> People -> People
		job alivePeople p = V.imap (\i a -> if age a == 20 then getAJob alivePeople chans a (V.unsafeSlice (i*off) off random) else a) p
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
		home alivePeople p = V.imap (\i a -> if age a == 20 then maybe (p ! i) (\a' -> getAHome mapRange center maps a (position a') (Xorshift $ random ! i)) (safeAccess p ((fromID.fst.parrents) a)) else a) p
			where
				random :: Vector Int64
				random = randomVector (V.length p) (pureMT $ fromIntegral $ fromXorshift gen)

				center :: (Float, Float)
				center = let l = V.filter (/=(0,0)) $ V.map position $ alivePeople in (\(x,y)->((fromIntegral x)/(int2Float $ V.length l), (fromIntegral y)/(int2Float $ V.length l))) $ V.foldr (\(x,y)(x',y')->(x+x',y+y')) (0,0) l

		maps :: VB.Vector (Vector Float)
		maps = VB.map perCulture allCulturesVector
			where
				perCulture :: Int -> Vector Float
				perCulture culture = concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture) .+. (culturalMap .! culture) .+. staticTerrainMap
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
					where f p = (fromIntegral . (\(x,y) -> x + y * range) . position $ p, id p)
--				peopleMap = VB.fromList $ loopPartition 0 (V.length positionMap) $ V.map f $ V.filter ((/=(0,0)).position) alivePeople
--					where
--					loopPartition :: Int -> Int -> Vector (Int, ID) -> [(Vector ID)]
--					loopPartition i max p
--						| i < max = let (a,b) = V.unstablePartition ((==max).fst) p in V.map snd a : loopPartition (i+1) max b
--						| otherwise = []
--					f p = (((\(x,y) -> x + y * range).position) p, id p)


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


createRelations :: Xorshift -> Friends -> People -> People -> VB.Vector (Vector ID) -> VB.Vector (Vector Float) -> VB.Vector (Vector ID) -> VB.Vector (Vector Float) -> (People, Friends)
createRelations gen friendss people alivePeople professionals professionalRelations culturals culturalRelations = unsafePerformIO $ do
	let s = V.length people
--	If fixed the result will not change depending on number of threads
	let offset = 12 --numCapabilities
	let randomGenerators = map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)
	let genWithRanges = zip randomGenerators $ let t = [0, s `div` offset..s-offset] ++ [s] in zip t (tail t)

	f <- VB.unsafeThaw friendss
--	fRef <- newMVar f
	applyFriendMatches f genWithRanges
--	t1 <- takeMVar fRef
	f' <- VB.unsafeFreeze f

	v <- V.unsafeThaw people
--	vRef <- newMVar v
	applyLoveMatches v f' genWithRanges
--	t2 <- takeMVar vRef
	v' <- V.unsafeFreeze v

	return (v', f')
	where
	applyFriendMatches :: MB.MVector RealWorld (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyFriendMatches fends list = P.mapM (\(gen, (from, to)) -> applyMatches from to gen) list
--	applyFriendMatches fends list = mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i max gen
			| i >= max = return ()
			| (not.alive) person = applyMatches (i+1) max gen
			| otherwise = do
--				withMVar fRef (\fends -> do
				friend <- MB.read fends i;
				if V.length friend + V.length m < maxFriends then do
					MB.write fends i (friend V.++ m);
				else do
					MB.write fends i ((V.drop (V.length m) friend) V.++ m );
				applyMatches (i+1) max gen'
			where
			person = people ! i

			maxFriends = 100 + ((fromIntegral . fromXorshift . step . Xorshift . fromID. id $ person) `rem` 200)

			-- Pick friends
			m :: Vector ID
			(m, gen') = match i person gen

	applyLoveMatches :: M.MVector RealWorld Person -> VB.Vector (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyLoveMatches v fends list = P.mapM (\(gen, (from, to)) -> applyMatches from to gen) list
--	applyLoveMatches vRef fends list = mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i max gen
			| i >= max = return ()
			| not . alive $ person = next
			| V.null friends = next
			| (==female) . gender .&&. (==0) . lover .&&. (<41) . age $ person = do
				when ((not.V.null) potentialLovers) $ do
					i' <- (return $ fromID $ m - start);

--					withMVar vRef (\v -> do
					p' <- M.read v i';
					p <- M.read v i;
					when (lover p' == 0 && lover p == 0) $ do
						M.write v i' (p' {lover = start + (toID i)});
						M.write v i (p {lover = m});
				next
			| otherwise = next
			where
			start = id $ V.head people
			next = applyMatches (i+1) max gen'
			person = people ! i
			friends = fends .! i

			potentialLovers :: Vector ID
			potentialLovers = V.filter (conditions . (people &!)) $ V.filter ((>=0) . (+(-start))) friends

			{-# INLINE conditions #-}
			conditions x = alive .&&. ((10<) .&&. (<50)) . age .&&. (==0) . lover .&&. ((/=) (parrents person)) . parrents $ x

			m = potentialLovers ! r
			(r, gen') = randomR (0, V.length potentialLovers-1) gen

	{-# INLINE match #-}
	match :: Int -> Person -> PureMT -> (Vector ID, PureMT)
	match personIndex person gen
		| V.null potentialMates = (V.empty, gen)
		| otherwise = (\(r, g)-> (V.map (potentialMates !) $ scale r (V.length potentialMates-1), g)) $ randomVector_ (timeStep*4) gen'
		where
			potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture V.++ potentialFriendsFriend

			(!random, gen') = randomVector_ (timeStep*2) gen

			scale :: Vector Double -> Int -> Vector Int
			scale v max = V.map (floor.(* (int2Float max)).double2Float) v

			potentialRandom = V.map (id . (alivePeople !)) $ scale (V.slice 0 timeStep random) (V.length alivePeople-1)
			potentialSameProfessional = V.map (prof !) $ scale random (V.length prof-1)
			potentialSameCulture = V.map (cult !) $ scale random (V.length cult-1)

			potentialProfessional :: Vector ID
			potentialProfessional = assert (V.sum profs <= V.length random) $ f 0 V.empty 0
				where
					f :: Int -> Vector ID -> Int -> Vector ID
					f i v offset
						| i >= V.length profs = v
						| V.null p || profs ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector ID
							result = V.map (p !) $ scale (V.slice offset (profs ! i) random) (V.length p-1)
							offset' = offset + profs ! i
							p = professionals .! i
			
			profs = V.map (floor . (* (int2Float $ timeStep*2))) $ normalize $ professionalRelations .! (professionToInt $ profession person)

			potentialCulture :: Vector ID
			potentialCulture = assert (V.sum cults <= V.length random) $ f 0 V.empty 0
				where
					f :: Int -> Vector ID -> Int -> Vector ID
					f i v offset
						| i >= V.length cults = v
						| V.null p || cults ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector ID
							result = V.map (p !) $ scale (V.slice offset (cults ! i) random) (V.length p-1)
							offset' = offset + cults ! i
							p = culturals .! i

			cults = V.map (floor.(* (int2Float $ timeStep*2))) $ normalize $ culturalRelations .! (cultureToInt $ culture person)

			normalize v = V.map (/ (V.sum v)) v

			prof :: Vector ID
			prof = professionals .! (professionToInt $ profession person)

			cult :: Vector ID
			cult = culturals .! (cultureToInt $ culture person)

			-- Pick friends from friends friends
			potentialFriendsFriend :: Vector ID
			potentialFriendsFriend
				| V.null friends = V.empty
				| otherwise = V.zipWith (\r friendID -> let ff = friendss .! (fromID $ friendID - start) in ff ! (floor $ (double2Float r) * (fromIntegral $ V.length ff-1))) r2 $ V.filter (>= start) $ V.map (friends !) $ scale r1 (V.length friends-1)
				where
				start = id $ V.head people

				r1 = V.slice 0 timeStep random
				r2 = V.slice timeStep timeStep random

				friends :: Vector ID
				friends = friendss .! personIndex



getAJob :: People -> [Double] -> Person -> Vector Double -> Person
getAJob people chans person random
	| profession person == none = person {profession = f allProfessions random chans}
	| otherwise = person
	where
	i = professionToInt . profession . (people !) . fromID . snd . parrents $ person
	chans' = (take i chans) ++ ((chans' !! i) + 0.2) : (drop (i+1) chans)

	f prof random chans
		| chans !! 0 >= random ! (0 :: Int) = prof !! 0
		| chans !! 1 >= random ! (1 :: Int) = prof !! 1
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
		| otherwise = person {position = home}
		where home = L.maximumBy (\a b -> compare (valueAt a) (valueAt b)) [possibleHomes !! i | i <- take 10 $ (randomRs (0, length possibleHomes-1) gen)]
	map = maps .! (cultureToInt $ culture person)
	possibleHomes = filter ((/= infinity) . valueAt) [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
	range' = 3
	valueAt p@(x,y) = (map ! (x+y*range)) + (professionValue $ profession person) * (scaleDistanceFromCenter $ distanceTo center (toFloat p))


toBuckets :: VB.Vector Int -> (Person -> Int) -> People -> VB.Vector (Vector ID)
toBuckets buckets test list = (VB.map (f list) buckets) `using` (parVector 1) --VB.map (f list) buckets
	where f list i = V.map id $ V.filter ((==i).test) list
--toBuckets size f people = VB.accumulate V.snoc (VB.replicate size V.empty) $ VB.map f people


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
	lovers list i = V.length $ V.filter ((==i) . getGroup . (people &!) . lover) $ V.filter ((/=0) . lover .&&. ((id . V.head $ people)<) . lover) $ V.map (people &!) list

	statisticalSample :: Int -> Vector ID
	statisticalSample i = V.map ((groupBuckets .! i) !) $ V.fromList $ take sampleSize (randomRs (0, VB.length groupBuckets - 1) gen :: [Int])

