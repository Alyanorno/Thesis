module Death where

import Prelude hiding (id)
import GHC.Float

import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import System.Random.Mersenne.Pure64 as R

import Stuff
import Definitions

death :: Options -> Xorshift -> (People, Friends) -> (People, Friends)
death opt gen (people, friends) = (V.zipWith testIfDead people' $ randomNumbers gen, friends')
	where
		friends' = VB.take (V.length people') friends
		randomNumbers = V.fromList . map double2Float . take (V.length people') . f . pureMT . fromIntegral . fromXorshift
			where f g = let (v,g') = R.randomDouble g in v : f g'

		people' = V.filter ((<80).age) $ V.map (\a -> a {age = age a + fromIntegral (timeStep opt)}) people
		testIfDead :: Person -> Float -> Person
		testIfDead p r
			| a < 20 = if r < 0.992414 .^ timeStep' then p else p {dead = 1} 
			| a < 30 = if r < 0.997491 .^ timeStep' then p else p {dead = 1}
			| a < 40 = if r < 0.997491 .^ timeStep' then p else p {dead = 1}
			| a < 50 = if r < 0.994962 .^ timeStep' then p else p {dead = 1}
			| a < 60 = if r < 0.992414 .^ timeStep' then p else p {dead = 1}
			| a < 70 = if r < 0.987259 .^ timeStep' then p else p {dead = 1}
			| a < 80 = if r < 0.974004 .^ timeStep' then p else p {dead = 1}
			| otherwise = p {dead = 1}
			where a = age p; (.^) = (**)
		timeStep' :: Float
		timeStep' = fromIntegral $ timeStep opt

