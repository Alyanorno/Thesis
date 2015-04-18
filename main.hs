{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Unboxed ((!), toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Array.Unboxed as A

import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad.ST()

import System.IO

import Stuff
import Birth
import Change


main = do
-- {-
	n1 <- getLine
	n2 <- getLine
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let (g,m) = (generations seed 0 ((start peopleFromStart, V.empty))) !! index
			let g' = VB.filter alive g
			let positions = [[VB.length $ VB.filter (\(x',y') -> x == x' && y == y') (VB.map position g') | x <- [0..mapRange]] | y <- [0..mapRange]]
			mapM_ (hPutStrLn h) $ map (foldr (\a list -> (show a) ++ " " ++ list) "") positions ))
		$ zip range $ map (\i -> "data/populationMap" ++ (show i) ++ ".txt") range
-- -}
{-
	n <- getLine
	let (g,m) = (generations seed 0 ((start peopleFromStart), V.empty)) !! read n
	let g' = VB.filter alive g
	putStrLn ""
	print $ VB.length g
	print $ VB.length g'
	print $ VB.length $ VB.filter ((/=0).lover) g'
-}
{-
	let positions = [[V.length $ V.filter (\(x',y') -> x == x' && y == y') (V.map position g') | x <- [0..mapRange]] | y <- [0..mapRange]]
	mapM_ print positions
--	withFile "test.txt" WriteMode (\h -> mapM_ (hPutStrLn h) $ map (\pos -> foldr (\s p -> s ++ " " ++ p) "" $ map show pos) positions)
	putStrLn "---------------------"
	mapM_ print $ chunksOf mapRange $ map round $ V.toList m
	putStrLn "---------------------"
-}
--	print g'
	putStrLn "---------------------"
	main

generations :: StdGen -> Int -> (People, Vector Float) -> [(People, Vector Float)]
generations seed i previous@(people,_) = previous : next
	where 
--		people = previous
		next = generations seed (i+1) $ ((change gen).(birth gen).(death gen)) people
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i -- xor seed i


death :: RandomGenerator -> People -> People
death gen people = VB.map f $ VB.zip people' $ r
	where
		r = VB.fromList $ map double2Float $ take (VB.length people') $ f $ pureMT $ fromIntegral $ fromXorshift gen
			where f g = let (v,g') = R.randomDouble g in v : f g'

		-- 140
		people' = VB.filter ((<140).age) $ VB.map (\a -> a {age = (age a) + timeStep}) people
		f :: (Person, Float) -> Person
		f (p,r)
			-- 0.7 0.95 0.95 0.80 0.70 0.50
			| a < 20 = if r < 0.97 .^ timeStep' then p else p {alive = False} 
			| a < 30 = if r < 0.99 .^ timeStep' then p else p {alive = False}
			| a < 40 = if r < 0.99 .^ timeStep' then p else p {alive = False}
			| a < 50 = if r < 0.98 .^ timeStep' then p else p {alive = False}
			| a < 60 = if r < 0.97 .^ timeStep' then p else p {alive = False}
			| a < 70 = if r < 0.95 .^ timeStep' then p else p {alive = False}
			| otherwise = p {alive = False}
			where a = age p; (.^) a b = (**) a b
		timeStep' :: Float
		timeStep' = fromIntegral timeStep

seed :: StdGen
seed = mkStdGen 0


