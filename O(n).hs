{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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




data Connection = Parrent | Child | Sibling | Lover deriving (Show, Eq, Ord)
data Relation = Relation { connection :: Connection, to :: ID } deriving (Show, Eq)
type Relations = [Relation]

type ID = Int
data Gender = Male | Female deriving (Show, Eq)
data Proffesion = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
data Person = Person { id :: ID, age :: Int, gender :: Gender, proffesion :: Proffesion, relations :: Relations } deriving (Show)
type People = Vector Person

instance Eq Person where
	p1 == p2 = id p1 == id p2




newtype Xorshift = Xorshift Int64 deriving (Show, Eq, Enum, Bounded)

step :: Xorshift -> Xorshift
step (Xorshift a) = Xorshift d where
	b = xor a (shiftL a 13)
	c = xor b (shiftR b 7)
	d = xor c (shiftL c 17)

instance RandomGen Xorshift where
	next a = (fromIntegral c, b)
		where b@(Xorshift c) = step a

	split  = error "Splitting on Xorshift not implemented"

	genRange a = (fromEnum (asTypeOf minBound a), fromEnum (asTypeOf maxBound a))


type RandomGenerator = Xorshift
--type RandomGenerator = StdGen



randomListsOf n gen = chunksOf 5 (randomRs (0, 1) gen :: [Float])
randomListsOf_ n gen = chunksOf 5 (randomRs (0, 1) gen :: [Float])

rescale :: Int -> Int -> Int -> Int
rescale maxX maxY a = floor $ (fromIntegral a) * ((fromIntegral maxY) / (fromIntegral maxX))

inRelation :: Relations -> Bool
inRelation [] = False
inRelation a = ((==Lover).connection.head) a



birth :: Int -> RandomGenerator -> People -> People
birth iteration gen people = fromList $ p ++ b
	where
		(p, b) = f (toList people) (randomListsOf 5 gen) $ chunksOf 5 [iteration*50000..]

		f :: [Person] -> [[Float]] -> [[Int]] -> ([Person], [Person])
		f [] _ _ = ([],[])
		f (person:people) randomLists ids
			| isBabyMaker person = let
				next = f people (tail randomLists) (tail ids)
				(female, children) = makeBabies person (head randomLists) (head ids)
				in (female : fst next, children ++ snd next)
			| otherwise = let next = f people randomLists ids
				in (person : fst next, snd next)
			where
				isBabyMaker person = gender person == Female && inRelation (relations person)

		makeBabies :: Person -> [Float] -> [Int] -> (Person, [Person])
		makeBabies female randomList ids = foo female babies
			where
				babies = map fst $ filter f $ zip possibleBabies randomList
					where f (b, p) = if p > 0.5 then True else False

				possibleBabies = [Person i 0 g None [] | (i, g) <- zip ids (cycle [Female, Male])]

				foo :: Person -> [Person] -> (Person, [Person])
				foo parrent [] = (parrent, [])
				foo parrent children = (addRelations (parrent, parrentToChild), map addRelations $ zip children (zipWith (:) childToParrent siblingRelations))
					where
						parrentToChild = [Relation Parrent (id c) | c <- children]
						childToParrent = [Relation Child (id parrent) | c <- children]

						siblingRelations = [[Relation Sibling (id (children!!i)) | i' <- [0..length children], i /= i'] | i <- [0..length children]]

				addRelations :: (Person, Relations) -> Person
				addRelations (p, r) = Person (id p) (age p) (gender p) (proffesion p) (relations p ++ r)



death :: People -> People
death people = V.filter ((<60).age) people




toBuckets :: (Enum e) => Int -> (Person -> e) -> People -> Vector People
toBuckets size test list = V.map (f list) $ fromList [0..size]
	where f list i = V.filter ((==i).fromEnum.test) list

change :: RandomGenerator -> People -> People
change gen people = (job.love.aged) people
	where
		aged people = V.map (\a -> a { age = age a + 10 }) people
		job people = V.zipWith getAJob people randomFloats2
		love people = V.zipWith3 f randomInts randomFloats people

		size = V.length people
		randomInts = fromList $ take size $ chunksOf 10 (randomRs (0, size - 1) gen :: [Int])
		randomFloats = fromList $ take size $ randomListsOf_ 11 gen
		randomFloats2 = fromList $ take size $ randomListsOf_ 2 gen

		f :: [Int] -> [Float] -> Person -> Person
		f randomInts randomFloats person
			| inRelation (relations person) = breakUp
			| otherwise = m
			where
				breakUp = if head randomFloats > 0.9 then m else person
				m = match person people randomInts (tail randomFloats)

		match :: Person -> People -> [Int] -> [Float] -> Person
		match person people randomInts randomFloats
			| length mates == 0 = person
			| otherwise = person { relations = Relation Lover (id (head mates)) : relations person }
			where
				mates = map fst $ filter (\(p, r) -> r > 0.7) $ zip potential randomFloats
				potential = potentialRandom ++ potentialProffesional
				potentialRandom = [p | i <- drop 5 randomInts, let p = people ! i, gender person /= gender p, (not.inRelation.relations) p]
				prof :: Vector Person
				prof = proffesionals ! (fromEnum $ proffesion person)
				potentialProffesional = [p | i <- map (rescale (V.length people) (V.length prof)) $ take 5 randomInts, let p = prof ! i, gender person /= gender p, (not.inRelation.relations) p]

		getAJob :: Person -> [Float] -> Person
		getAJob person random
			| proffesion person == None = person { proffesion = f random }
			| otherwise = person
			where f random
				| chansFarmer > random !! 0 = Farmer
				| chansAdministrator > random !! 1 = Administrator
				| otherwise = Beggar

		chansFarmer = demand Farmer 3
		chansAdministrator = f (V.length people)
			where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1

		demand :: Proffesion -> Float -> Float
		demand work ratio = (((fromIntegral (V.length people)) / n) / ratio) / 2
			where n = fromIntegral $ V.length $ V.filter ((==work).proffesion) people

		proffesionals = toBuckets (fromEnum (maxBound :: Proffesion)) proffesion people


--		enumToList :: (Enum a, Bounded a) => [a]
--		enumToList = [minBound..]

--		enumSize :: (Enum a, Bounded a) => Int
--		enumSize = length [minBound..]



--toBuckets :: (Enum e) => Int -> (Person -> e) -> Vector Person -> Vector (Vector Person)
--toBuckets size test list = V.accumulate (V.++) buckets $ V.map (placeInBucket buckets) list
--	where
--		placeInBucket :: Vector (Vector Person) -> Person -> (Int, Vector Person)
--		placeInBucket b a = (i, V.snoc (b ! i) a)
--			where i = fromEnum (test a)

--		buckets :: Vector (Vector Person)
--		buckets = V.replicate size V.empty


--	genRange a = (fromEnum (asTypeOf minBound a), fromEnum (asTypeOf maxBound a))



--toBucketsFast :: (Enum e) => Int -> (Person -> e) -> Vector Person -> Vector (Vector Person)
--toBucketsFast size test list = runST $ do
--	buckets <- M.new (length list) -- :: Vector (Vector Person)
--	return buckets




generations :: Int -> Int -> People -> [People]
generations seed i first = first : (generations seed (i+1) $ ((birth i gen).death.(change gen)) first)
	where gen = Xorshift $ fromIntegral $ xor seed i
--	where gen = mkStdGen $ fromIntegral $ xor seed i


start :: Int -> People
start a = fromList [Person i 20 g None [] | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 

seed :: Int
seed = 0

main = do
	n <- getLine
	let g = (generations seed 0 (start 5)) !! read n
	putStrLn ""
	print $ V.length g
	print $ toList $ V.map V.length $ toBuckets (fromEnum (maxBound :: Proffesion)) proffesion g
--	print $ map age $ g
--	print $ length $ filter ((==Lover).connection) $ snd $ g
--	print $ length $ filter ((==Sibling).connection) $ snd $ g
--	print $ length $ snd $ g
--	print $ [length $ filter ((==Farmer).proffesion) $ fst g, length $ filter ((==Administrator).proffesion) $ fst g, length $ filter ((==Beggar).proffesion) $ fst g]
	putStrLn "---------------------"
	main

