import Prelude hiding (id)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (partition, find, sort)
import Data.Maybe (isNothing)
import Data.Function (fix)


type ID = Int
data Gender = Male | Female deriving (Show, Eq)
data Person = Person { id :: ID, age :: Int, gender :: Gender } deriving (Show)
type People = [Person]

data Connection = Parrent | Sibling | Lover deriving (Show, Eq, Ord)
data Relation = Relation { connection :: Connection, persons :: (ID, ID) } deriving (Show, Eq)
type Relations = [Relation]

instance Ord Relation where
	compare r1 r2 = compare (fst (persons r1), snd (persons r1), connection r1) (fst (persons r2), snd (persons r2), connection r2)

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
birth lists = (fst r, snd lists ++ snd r)
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
		lovers = filter ((==Lover).connection) (snd lists)


death :: (People,Relations) -> (People,Relations)
death lists = (p, r)
	where
		(p, dead) = partition ((<60).age) (fst lists)
		r = filter f (snd lists)
		f a | isNothing found = False | otherwise = True
			where found = find (\d -> fst (persons a) == d || snd (persons a) == d) (map id dead)


mapTuple f (a1, a2) = (f a1, f a2)

change :: (People,Relations) -> (People,Relations)
change lists = (p, r)
	where
		p :: People
		p = map (\a -> Person (id a) (age a + 10) (gender a)) (fst lists)

		r :: Relations
		r = map fst $ joinProbability new base -- TODO: Do the probability

		joinProbability :: (Num a) => [(Relation, a)] -> [(Relation, a)] -> [(Relation, a)]
		joinProbability a b
			| length a == 0 = b
			| length b == 0 = a

			| x == y = (fst (head a), snd (head a) * snd (head b)) : joinProbability (tail a) (tail b)
			| x > y = joinProbability a (tail b)
			| x < y = joinProbability (tail a) b
			where (x, y) = mapTuple (fst.head) (a, b)


		new = zip (sort (snd lists)) (cycle [0.5]) -- TOOD: Complete
		base = zip (snd lists) (cycle [0.5])


generations :: [(People,Relations)] -> [(People,Relations)]
generations first = first ++ fix (\f -> [(birth.death.change) ((head.(drop i)) f) | i <- [0..]])





start :: Int -> People
start a = [Person i 20 g | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 


main = do
	n <- getLine
	putStrLn $ show $ fst $ generations [(start 5, [])] !! read n
	main


