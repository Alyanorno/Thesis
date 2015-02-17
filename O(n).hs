import Prelude hiding (id)

import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor)
import System.Random




data Connection = Parrent | Child | Sibling | Lover deriving (Show, Eq, Ord)
data Relation = Relation { connection :: Connection, to :: ID } deriving (Show, Eq)
type Relations = [Relation]

type ID = Int
data Gender = Male | Female deriving (Show, Eq)
data Proffesion = Farmer | Administrator | Beggar | None deriving (Show, Eq)
data Person = Person { id :: ID, age :: Int, gender :: Gender, proffesion :: Proffesion, relations :: Relations } deriving (Show)
type People = [Person]

instance Eq Person where
	p1 == p2 = id p1 == id p2



randomListsOf n gen = chunksOf 5 (randomRs (0, 1) gen :: [Float])



inRelation :: Relations -> Bool
inRelation [] = False
inRelation a = ((==Lover).connection.head) a



birth :: Int -> StdGen -> People -> People
birth _ _ [] = []
birth iteration gen people = p ++ b
	where
		(p, b) = f people (randomListsOf 5 gen) $ chunksOf 5 [iteration*50000..]

		f :: People -> [[Float]] -> [[Int]] -> (People, People)
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

		makeBabies :: Person -> [Float] -> [Int] -> (Person, People)
		makeBabies female randomList ids = foo female babies
			where
				babies = map fst $ filter f $ zip possibleBabies randomList
					where f (b, p) = if p > 0.5 then True else False

				possibleBabies = [Person i 0 g None [] | (i, g) <- zip ids (cycle [Female, Male])]

				foo :: Person -> People -> (Person, People)
				foo parrent [] = (parrent, [])
				foo parrent children = (addRelations (parrent, parrentToChild), map addRelations $ zip children (zipWith (:) childToParrent siblingRelations))
					where
						parrentToChild = [Relation Parrent (id c) | c <- children]
						childToParrent = [Relation Child (id parrent) | c <- children]

						siblingRelations = [[Relation Sibling (id (children!!i)) | i' <- [0..length children], i /= i'] | i <- [0..length children]]

				addRelations :: (Person, Relations) -> Person
				addRelations (p, r) = Person (id p) (age p) (gender p) (proffesion p) (relations p ++ r)



death :: People -> People
death people = filter ((<60).age) people



change :: StdGen -> People -> People
change _ [] = []
change gen people = f (map (\a -> a { age = age a + 10 }) people) gen
	where
		f :: People -> StdGen -> People
		f [] _ = []
		f (person:xs) gen
			| inRelation (relations person) = breakUp : f xs gen'
			| otherwise = match person people gen : f xs gen'
			where
				breakUp = if r > 0.9 then match person people gen else person
				(r, gen') = (randomR (0, 1) gen :: (Float, StdGen))

		match :: Person -> People -> StdGen -> Person
		match person people gen
			| length mates == 0 = person
			| otherwise = person { relations = Relation Lover (id (head mates)) : relations person }
			where
				potential = [p | i <- take 10 (randomRs (0, length people - 1) gen :: [Int]), let p = people !! i, gender person /= gender p, (not.inRelation.relations) p]
				mates = map fst $ filter (\(p, r) -> r > 0.7) $ zip potential (randomRs (0, 1) gen :: [Float])








--randomRsg :: (RandomGen g, Random a) => Int -> (a, a) -> g -> ([a], g)
--randomRsg antal range gen = (take antal $ randomRs range gen, snd $ split gen)


--change :: StdGen -> (People,Relations) -> (People,Relations)
--change _ ([], _) = ([], [])
--change randGen (people, relations) = (p, r)
--	where
--		p :: People
--		p = map (\(a, r) -> Person (id a) (age a + 10) (gender a) (getAJob a r)) $ zip people $ chunksOf 2 (randomRs (0.0, 1.0) (snd (split randGen)) :: [Float])
--
--		getAJob :: Person -> [Float] -> Proffesion
--		getAJob person r
--			| proffesion person == None = f p r
--			| otherwise = proffesion person
--			where f p r
--				| chansFarmer > r !! 0 = Farmer
--				| chansAdministrator > r !! 1 = Administrator
--				| otherwise = Beggar
--
--		chansFarmer = demand Farmer 3
--		chansAdministrator = f (length people)
--			where f x = 1.001 ^^ (0 - (x + 1000)) + 0.1
--
--		demand :: Proffesion -> Float -> Float
--		demand work ratio = (((fromIntegral (length people)) / n) / ratio) / 2
--			where n = fromIntegral $ length $ filter ((==work).proffesion) people







generations :: Int -> Int -> People -> [People]
generations seed i first = first : (generations seed (i+1) $ ((birth i gen).death.(change gen)) first)
	where gen = mkStdGen $ xor seed i


start :: Int -> People
start a = [Person i 20 g None [] | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 

seed :: Int
seed = 0

main = do
	n <- getLine
	let g = (generations seed 0 (start 5)) !! read n
	putStrLn ""
	print $ length $ g
--	print $ map age $ g
--	print $ length $ filter ((==Lover).connection) $ snd $ g
--	print $ length $ filter ((==Sibling).connection) $ snd $ g
--	print $ length $ snd $ g
--	print $ [length $ filter ((==Farmer).proffesion) $ fst g, length $ filter ((==Administrator).proffesion) $ fst g, length $ filter ((==Beggar).proffesion) $ fst g]
	putStrLn "---------------------"
	main

