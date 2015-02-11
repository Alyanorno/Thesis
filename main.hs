import Prelude hiding (id)
import Data.List (partition, find)
import Data.Maybe (isNothing)
import Data.Function (fix)

type ID = Int
data Gender = Male | Female deriving (Show, Eq)
data Person = Person { id :: ID, age :: Int, gender :: Gender } deriving (Show)
type People = [Person]

data Connection = Parrent | Sibling | Lover deriving (Show, Eq)
data Relation = Relation { connection :: Connection, persons :: (ID, ID) } deriving (Show)
type Relations = [Relation]



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


change :: (People,Relations) -> (People,Relations)
change a = (p, r)
	where
		p = map (\a -> Person (id a) (age a + 10) (gender a)) p
		r = (snd a) -- TODO: Complete



generations :: [(People,Relations)] -> [(People,Relations)]
generations first = first ++ fix (\f -> [(birth.death.change) ((head.(drop i)) f) | i <- [0..]])





start :: Int -> People
start a = [Person i 20 g | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 


main = do
	n <- getLine
	putStrLn $ show $ fst $ generations [(start 5, [])] !! read n
	main


