



type ID = Int
data Person = Person { id :: ID } deriving (Show)
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
birth a = (fst r, snd latest ++ snd r)
	where 
		r = foldr1 (\a b -> (fst a ++ fst b, snd a ++ snd b)) $ map f lovers
			where
				f l = (p, c)
				p = [Person i | i <- [0..3]] -- TODO: Must change to proper id
				c = parrents ++ siblings
				parrents = [[Relation Parrent ((id c), (id p)) | c <- p] | p <- [fst l, snd l]]
				siblings = foo p
					where foo s
						| length s > 1 = [Relation Sibling (id (head s)) c | c <- (tail s)] ++ foo (tail s)
						| otherwise = []
		latest = (last a)
		lovers = filter ((==Lover).connection) (snd latest)


death :: [(People,Relations)] -> [(People,Relations)]
death a = a


change :: [(People,Relations)] -> [(People,Relations)]
change a = a



generations :: [(People,Relations)] -> [(People,Relations)]
generations first = first ++ [(birth.death.change) (((take 2).(drop i)) generations) | i <- [0..]]





start :: Int -> Int -> [People]
start a b = [[Person i + l | i <- [0..b]] | l <- [0..a]]


main = do
	n <- getLine
	putStrLn $ show $ fst $ generations (start 1 5) !! read n
	main


