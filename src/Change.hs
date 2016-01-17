{-# LANGUAGE DataKinds, FlexibleContexts, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Change (change, toBuckets) where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import qualified Data.List as L
import Foreign.Storable.Tuple ()

import Control.Monad.State.Lazy --(get, put, evalState, State(..))
import Control.Monad.ST

import Data.Function (on)
import Data.Int (Int32, Int64)
import System.Random
import System.Random.Mersenne.Pure64 as R

import Stuff
import Definitions
import Relations



change :: Options -> Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
change opt gen (people, friends, childrens) = let (p, f) = relations alivePeople friends . home alivePeople . job alivePeople $ people in (p, f, childrens)
	where
		alivePeople = V.filter alive people

		relations :: People -> Friends -> People -> (People, Friends)
		relations alivePeople friends p = createRelations opt gen friends p alivePeople professionalBuckets professionalRelations cultureBuckets culturalRelations

		job :: People -> People -> People
		job alivePeople p = runST $ do
			v <- V.unsafeThaw p
			fix (\loop i l -> when (i /= V.length p) $ do
				let r = V.unsafeSlice (l*off) off random
				a <- MV.read v i
				if age a == 20
					then do MV.write v i (getAJob alivePeople chans a r); loop (i+1) (l+1)
					else loop (i+1) l
				) 0 0
			v' <- V.unsafeFreeze v
			return v'
			where
 				(random,_) = randomVector_ (size*off) (pureMT $ fromIntegral $ fromXorshift gen)
					where size = V.length . V.filter ((==20).age) $ p
				off = 2

				chans :: [Double]
 				chans = map float2Double [chansFarmer, chansAdministrator, chansBeggar, 0]
				chansFarmer = 0.3 --demand farmer 3
					where
						demand :: Profession -> Float -> Float
						demand work ratio = ((fromIntegral (V.length p) / n) / ratio) / 2
							where n = fromIntegral $ V.length $ V.filter ((==work).profession) p
				chansAdministrator = 0.4 -- f (V.length p)
					where f x = 1.001 ^^ negate (x + 1000) + 0.1 -- 0.1
				chansBeggar = 1

		home :: People -> People -> People
		home alivePeople p = runST $ do
			v <- V.unsafeThaw p
			fix (\loop i l -> when (i /= V.length p) $ do
				a <- MV.read v i
				if age a == 20
					then do MV.write v i (maybe (p ! i) (\a' -> getAHome opt center maps a (position a') (Xorshift $ random ! l)) (safeAccess p ((fromID.fst.parrents) a))); loop (i+1) (l+1)
					else loop (i+1) l
				) 0 0
			v' <- V.unsafeFreeze v
			return v'
			where
				random :: Vector Int64
				random = randomVector size (pureMT $ fromIntegral $ fromXorshift gen)
					where size = V.length . V.filter ((==20).age) $ p

				center :: (Float, Float)
				center = {-toFloat (startPosition opt)-} let l = V.filter (/=(0,0)) $ V.map position alivePeople in (\(x,y)->(fromIntegral x / int2Float (V.length l), fromIntegral y / int2Float (V.length l))) $ V.foldr (\(x,y)(x',y')->(x+x',y+y')) (0,0) l

		maps :: Int -> Int -> Vector Float
		maps profession' culture' = V.zipWith (\a b -> if a == impossiblePos then impossiblePos else a+b) (staticTerrainMap opt) $ concentrationOfPeopleMap .+. (distanceFromCulturalCenter .! culture') .+. (culturalMap .! culture') .+. (professionalMap .! profession')
			where
			(.+.) = V.zipWith (+)

			distanceFromCulturalCenter :: VB.Vector (Vector Float)
			distanceFromCulturalCenter = VB.map (V.map $ scaleDistanceFromCulturalCenter opt) $
				VB.map (\i -> let
					peoplePos = V.map (position . (people &!)) $ cultureBuckets .! i
					average :: (Int32,Int32) -> (Float,Float); average (x,y) = let l = V.length peoplePos in if l == 0 then (0,0) else (fromIntegral x / int2Float (V.length peoplePos), fromIntegral y / int2Float (V.length peoplePos))
					culturalCenter = average $ V.foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) peoplePos
					in V.map (distanceTo culturalCenter . toFloat) positionMap) allCulturesVector

			concentrationOfPeopleMap :: Vector Float
			concentrationOfPeopleMap = V.generate (VB.length peopleMap) (\i -> (scaleConcentrationOfPeople opt.fromIntegral.V.length) (peopleMap .! i))

			culturalMap :: VB.Vector (Vector Float)
			culturalMap = VB.map (\i -> boxFilter $ V.generate (VB.length peopleMap) (\l -> (scaleCulturalMap opt . f i) (peopleMap .! l))) allCulturesVector
				where 
					f i p = let m = V.map (relationsTo i) p; l = V.length m in if l == 0 then 0 else V.sum m / int2Float l

					relationsTo :: Int -> ID -> Float
					relationsTo i p = (culturalRelations .! i) ! (cultureToInt . culture . (people &!) $ p)

			professionalMap :: VB.Vector (Vector Float)
			professionalMap = VB.map (\i -> V.generate (VB.length peopleMap) (\l -> (scaleProfessionalMap opt . f i) (peopleMap .! l))) allProfessionsVector
				where 
				f i p = let m = V.map (relationsTo i) p; l = V.length m in if l == 0 then 0 else V.sum m / int2Float l

				relationsTo :: Int -> ID -> Float
				relationsTo i p = (professionalRelations .! i) ! (professionToInt . profession . (people &!) $ p)

			peopleMap :: VB.Vector (Vector ID)
			peopleMap = VB.unsafeAccumulate V.snoc (VB.replicate (V.length positionMap) V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert alivePeople
				where f p = (fromIntegral . (\(x,y) -> x + y * range) . position $ p, id p)

			positionMap :: Vector (Int32, Int32)
			positionMap = V.fromList [(x,y) | x <- [0..range-1], y <- [0..range-1]]

		range = mapRange opt

		numberProfessions = length allProfessions
		numberCultures = length allCultures

		professionalBuckets :: VB.Vector (Vector ID)
		professionalBuckets = toBuckets allProfessionsVector (professionToInt.profession) alivePeople

		cultureBuckets :: VB.Vector (Vector ID)
		cultureBuckets = toBuckets allCulturesVector (cultureToInt.culture) alivePeople

		sampleSize = 100

		professionalRelations :: VB.Vector (Vector Float)
		professionalRelations = VB.zipWith (V.zipWith (+)) (staticProfessionalRelations opt) $ relationsBetween gen people numberProfessions (professionToInt . profession) professionalBuckets sampleSize

		culturalRelations :: VB.Vector (Vector Float)
		culturalRelations = relationsBetween gen people numberCultures (cultureToInt . culture) cultureBuckets sampleSize

getAJob :: People -> [Double] -> Person -> Vector Double -> Person
getAJob people chans person random
	| profession person == none = person {profession = f allProfessions random chans'}
	| otherwise = person
	where
	i = professionToInt . profession . (people &!) . snd . parrents $ person
	chans' = take i chans ++ (chans !! i) + 0.2 : drop (i+1) chans

	f prof random chans
		| head chans'  >= random ! (0 :: Int) = head prof
		| head (tail chans') >= random ! (1 :: Int) = head $ tail prof
		| otherwise = head $ tail $ tail prof


getAHome :: Options -> (Float,Float) -> (Int -> Int -> Vector Float) -> Person -> (Int32,Int32) -> Xorshift -> Person
getAHome opt center maps person parrentPosition gen
	| (>10) . age .&&. (/=0) . lover .&&. (==(0,0)) . position $ person = theHome
	| otherwise = person
	where
	theHome
		| parrentPosition == (0,0) = person {position = startPosition opt}
		| null possibleHomes = person {position = parrentPosition}
		| otherwise = person {position = home}
		where home = L.maximumBy (compare `on` valueAt) [possibleHomes !! i | i <- take (positionSampling opt) $ randomRs (0, length possibleHomes-1) gen]
--		where home = L.maximumBy (compare `on` valueAt) possibleHomes
	map = maps (professionToInt (profession person)) (cultureToInt (culture person))
	possibleHomes = filter ((/= impossiblePos) . valueAt) [(x,y) | let (x',y') = parrentPosition, x <- [x'-range'..x'+range'], y <- [y'-range'..y'+range'], x < range, x > 0, y < range, y > 0]
	range' = moveRange opt
	valueAt p@(x,y) = if posValue == impossiblePos then impossiblePos else posValue + 10 * (professionValue opt) (profession person) * (scaleDistanceFromCenter opt) (distanceTo center (toFloat p))
		where posValue = map ! (x+y*range)
	range = mapRange opt


toBuckets :: VB.Vector Int -> (Person -> Int) -> People -> VB.Vector (Vector ID)
toBuckets buckets test list = VB.map (f list) buckets --`using` (parVector 1) --VB.map (f list) buckets
	where f list' i = V.map id $ V.filter ((==i).test) list'
--toBuckets size f people = VB.accumulate V.snoc (VB.replicate size V.empty) $ VB.map f people

