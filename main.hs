import Prelude hiding (id)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor)
import System.Random



type ID = Int
data Gender = Male | Female deriving (Show, Eq)
data Person = Person { id :: ID, age :: Int, gender :: Gender } deriving (Show)
type People = [Person]

instance Eq Person where
	p1 == p2 = id p1 == id p2

data Connection = Parrent | Sibling | Lover deriving (Show, Eq, Ord)
data Relation = Relation { connection :: Connection, persons :: (ID, ID) } deriving (Show, Eq)
type Relations = [Relation]

instance Ord Relation where
	compare r1 r2 = compare (fst (persons r1), snd (persons r1), connection r1) (fst (persons r2), snd (persons r2), connection r2)



anyTuple f (a1, a2) = f a1 || f a2
mapTuple f (a1, a2) = (f a1, f a2)


-- List of people
-- Connections between people

-- Age people -> Update all connections
-- Remove People -> Update removed people connections
-- Add people -> Add connections to added people


-- Start next elemnt (age people) by updating connections
-- 	Create list over all possible relationships and their probability
-- 	Increase probability for already existing relationships
-- 	Run the probability engine
-- Check for age and death probability and remove dead people
-- Check for pair and birth probability and add new people


birth :: (People,Relations) -> (People,Relations)
birth (people, relations) = (fst r, relations ++ snd r)
	where 
		r = foldr1 (\a b -> (fst a ++ fst b, snd a ++ snd b)) $ map f lovers
			where
				f :: Relation -> (People, Relations)
				f l = (p, c)
					where 
						p = [Person i 0 g | (i,g) <- zip [0..3] (cycle [Male, Female])] -- TODO: Must change to proper id
						c = parrents ++ siblings
						parrents = concat [[Relation Parrent (c, t) | c <- map id p] | t <- [fst (persons l), snd (persons l)]]
						siblings = foo p
							where 
								foo :: People -> Relations
								foo s
									| length s > 1 = [Relation Sibling ((id (head s)), (id c)) | c <- tail s] ++ foo (tail s)
									| otherwise = []
		lovers = filter ((==Lover).connection) relations


randomRsg :: (RandomGen g, Random a) => Int -> (a, a) -> g -> ([a], g)
randomRsg antal range gen = (take antal $ randomRs range gen, snd $ next gen)


death :: (People,Relations) -> (People,Relations)
death (people, relations) = (p, r)
	where
		(p, dead) = partition ((<60).age) people
		r = filter f relations
		f a = isJust $ find (\d -> anyTuple (==d) $ persons a) $ map id dead



change :: StdGen -> (People,Relations) -> (People,Relations)
change randGen (people, relations) = (p, r)
	where
		p :: People
		p = map (\a -> Person (id a) (age a + 10) (gender a)) people

		r :: Relations
		r = remain ++ new

		new = f free randGen

		f :: People -> StdGen -> Relations
		f [] gen = []
		f people gen
			| isNothing mate = f (tail people) gen'
			| otherwise = Relation Lover (id person, id (fst (fromJust mate))) : f (tail $ delete (fst (fromJust mate)) people) gen'
			where
				mate = find ((>0.8).snd) $ zip potential (randomRs (0.0, 1.0) gen :: [Float])

				person = head people

				potential = [i | i <- map (people !!) t, gender i /= gender person]
				(t, gen') = randomRsg 5 (0, length people) gen :: ([Int], StdGen)


		lovers :: Relations
		lovers = filter ((==Lover).connection) relations

		remain :: Relations
		remain = map fst $ filter ((>0.1).snd) $ zip lovers (randomRs (0.0, 1.0) randGen :: [Float])

		free :: People
		free = filter (\p -> isNothing $ find (\r -> anyTuple (==id p) $ persons r) remain) people


generations :: Int -> [(People,Relations)] -> [(People,Relations)]
generations seed first = first ++ fix (\f -> [(birth.death.(change (mkStdGen (xor seed i)))) ((head.(drop i)) f) | i <- [0..]])





start :: Int -> People
start a = [Person i 20 g | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 

seed :: Int
seed = 0

main = do
	n <- getLine
	putStrLn $ show $ fst $ generations seed [(start 5, [])] !! read n
	main


