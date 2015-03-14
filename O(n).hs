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

import Stuff
import Birth
import Change


main = do
	n <- getLine
	let (g, _, _) = (generations seed 0 ((start 5), [(minBound :: Profession)..], [(minBound :: Culture)..])) !! read n
	putStrLn ""
	print $ V.length $ g
--	print $ toList $ V.map V.length $ toBuckets (fromEnum (maxBound :: Profession)) profession g
--	print $ map age $ g
--	print $ length $ filter ((==Lover).connection) $ snd $ g
--	print $ length $ filter ((==Sibling).connection) $ snd $ g
--	print $ length $ snd $ g
--	print $ [length $ filter ((==Farmer).proffesion) $ fst g, length $ filter ((==Administrator).proffesion) $ fst g, length $ filter ((==Beggar).proffesion) $ fst g]
	putStrLn "---------------------"
	main


generations :: Int -> Int -> (People, Professions, Cultures) -> [(People, Professions, Cultures)]
generations seed i previous@(people, professions, cultures) = previous : next
	where 
		next = generations seed (i+1) $ ((change gen professions cultures).(birth i gen).death) people
		gen = Xorshift $ fromIntegral $ xor seed i
--		gen = mkStdGen $ fromIntegral $ xor seed i


death :: People -> People
death people = V.filter ((<60).age) people


seed :: Int
seed = 0


