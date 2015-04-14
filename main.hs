{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

import Prelude hiding (id)
import GHC.Float

import Data.Vector ((!), fromList, toList, Vector)
import qualified Data.Vector as V

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad.ST

import System.IO

import Stuff
import Birth
import Change


--main = do
--	mapM_ (\(index, name) ->
--		withFile name WriteMode (\h ->
--			let (g,_,_) = (generations seed 0 ((start 5), [(minBound :: Profession)..], [(minBound :: Culture)..])) !! index in
--			mapM_ (hPutStrLn h) $ V.map (\(x,y) (x',y') -> (fromInteger x) : ' ' : (fromInteger y) : '\n' : (fromInteger x') : (fromInteger y') : []) $ V.map (\p -> (position p, position $ g ! ((id $ g ! 0) + lover p))) $ V.filter ((/=0).lover) g)) $
--		zip [0..10] $ map (\i -> "data" ++ (fromInteger i) : ".txt") [0..10]


main = do
	n <- getLine
	let (g,m) = (generations seed 0 ((start peopleFromStart), fromList [])) !! read n
	let g' = V.filter alive g
	putStrLn ""
	print $ V.length g
	print $ V.length g'
	print $ V.length $ V.filter ((/=0).lover) g'
	let positions = [[V.length $ V.filter (\(x',y') -> x == x' && y == y') (V.map position g') | x <- [0..mapRange]] | y <- [0..mapRange]]
	mapM_ print positions
	withFile "test.txt" WriteMode (\h -> mapM_ (hPutStrLn h) $ map (\pos -> foldr (\s p -> s ++ " " ++ p) "" $ map show pos) positions)
	putStrLn "---------------------"
	print $ V.length m
	mapM_ print $ chunksOf mapRange $ map round $ V.toList m
	putStrLn "---------------------"
--	print g'
--	putStrLn "---------------------"
	main

professions = [(minBound :: Profession)..]
cultures = [(minBound :: Culture)..]

generations :: Int -> Int -> (People, Vector Float) -> [(People, Vector Float)]
generations seed i previous@(people,_) = previous : next
	where 
--		people = previous
		next = generations seed (i+1) $ ((change gen professions cultures).(birth i gen).(death gen)) people
		gen = Xorshift $ fromIntegral $ xor seed i


death :: RandomGenerator -> People -> People
death gen people = V.map f $ V.zip people' $ r
--death gen people = V.map f $ V.zip people' $ V.map fst $ V.iterateN (V.length people') ((randomR (0,1)).snd) (randomR (0,1) gen :: (Float, RandomGenerator)) -- (randomRs (0,1) gen :: [Float])
	where
--		r = fromList $ take (V.length people') (randomRs (0,1) gen :: [Float])
		r = fromList $ map double2Float $ take (V.length people') $ f $ pureMT $ fromIntegral $ fromXorshift gen
			where f g = let (v,g') = R.randomDouble g in v : f g'

		people' = V.filter ((<140).age) people
		f :: (Person, Float) -> Person
		f (p,r)
			-- 0.7 0.95 0.95 0.80 0.70 0.50
			| a < 20 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False} 
			| a < 30 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False}
			| a < 40 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False}
			| a < 50 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False}
			| a < 60 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False}
			| a < 70 = if r < 10 .^ (log(0.95)*(timeStep'/10)) then p else p {alive = False}
			| otherwise = p {alive = False}
			where a = age p; (.^) a b = (**) a b
		timeStep' :: Float
		timeStep' = fromIntegral timeStep

toFloat :: Int -> Float
toFloat x = fromIntegral x

seed :: Int
seed = 0


