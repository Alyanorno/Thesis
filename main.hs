





type ID = Int
data Person = { id :: ID } deriving (Show)
data Relation = { id1 :: ID, id2 :: ID } deriving (Show)
type People = [Person]
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


birth :: [(People,Relations)] -> [(People,Relations)]
birth a = a

death :: [(People,Relations)] -> [(People,Relations)]
death a = a

change :: [(People,Relations)] -> [(People,Relations)]
change a = a


generations :: [(People,Relations)] -> [(People,Relations)]
generations first = first ++ [(birth . death . change) (((take 1).(drop i)) generations) | i <- [0..]]


