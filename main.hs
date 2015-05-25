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
import Data.List.Split (chunksOf)

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
-- Population Map
{-
	n1 <- getLine
	n2 <- getLine
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let (g,m) = (generations seed 0 ((start peopleFromStart, V.empty))) !! index
			let g' = VB.filter alive g
			let positions = [[((x,y), VB.length $ VB.filter (\(x',y') -> x == x' && y == y') (VB.map position g')) | x <- [0..mapRange]] | y <- [0..mapRange]]
			let f x--let r = x * 4 in if r > 255 then 255 else r
				| x == 0 = 0
				| x < 10 = 50
				| x < 50 = 50 + x * 3
				| otherwise = 200
			hPutStrLn h $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show $ 255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
		$ zip range $ map (\i -> "data/populationMap" ++ (show i) ++ ".txt") range
-}
-- Culture Map
{-
--	n1 <- getLine
--	n2 <- getLine
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let (g,m) = (generations seed 0 ((start peopleFromStart, V.empty))) !! index
			let g' = VB.filter alive g
			let f p c
				| VB.null p = 0
				| otherwise = float2Int $ (int2Float $ VB.length $ VB.filter ((==c).culture) p) / (int2Float $ VB.length p) * 255
			let positions = [[((x,y), f p Endorphi, f p Brahmatic)  | x <- [0..mapRange], let p = VB.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
			hPutStrLn h $ concat $ map (foldr (\((x,y),g,b) list -> (show x) ++ " " ++ (show y) ++ " " ++ "0" ++ " " ++ (show g) ++ " " ++ (show b) ++ " " ++ (show $ if g + b == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
		$ zip range $ map (\i -> "data/cultureMap" ++ (show i) ++ ".txt") range
-}
-- Profession Map
{-
--	n1 <- getLine
--	n2 <- getLine
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let (g,m) = (generations seed 0 ((start peopleFromStart, V.empty))) !! index
			let g' = VB.filter alive g
			let f l
				| VB.null l = 0
				| otherwise = (VB.foldr (\x list -> list + ((*) 100 $ professionValue x)) 0 (VB.map profession l)) `div` (VB.length l)
			let positions = [[((x,y), f p) | x <- [0..mapRange], let p = VB.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
			hPutStrLn h $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
		$ zip range $ map (\i -> "data/professionMap" ++ (show i) ++ ".txt") range
-}
--{-
	n <- getLine
	let (g,m) = (generations seed 0 ((start peopleFromStart), V.empty)) !! read n
	let g' = VB.filter alive g
	putStrLn ""
	print $ VB.length g - VB.length g'
	print $ VB.length g'
	print $ VB.length $ VB.filter (((==Female).gender) .&&. ((/=0).lover) .&&. ((<41).age)) g'
	print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g | let ages = [0,10..80], (min,max) <- zip ages (tail ages)]
	print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g' | let ages = [0,10..80], (min,max) <- zip ages (tail ages)]
---}
{-
	n <- getLine
	let (g,m) = (generations seed 0 ((start peopleFromStart), V.empty)) !! read n
	let g' = VB.filter alive g
	print $ VB.length $ VB.filter ((==Endorphi).culture) g'
	print $ VB.length $ VB.filter ((==Brahmatic).culture) g'
	let positions = [[VB.length $ VB.filter (\(x',y') -> x == x' && y == y') (VB.map position g') | x <- [0..mapRange]] | y <- [0..mapRange]]
	mapM_ print positions
--	withFile "test.txt" WriteMode (\h -> mapM_ (hPutStrLn h) $ map (\pos -> foldr (\s p -> s ++ " " ++ p) "" $ map show pos) positions)
	putStrLn "---------------------"
	mapM_ print $ chunksOf mapRange $ map round $ V.toList m
-}
--	putStrLn "---------------------"
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

		people' = VB.filter ((<120).age) $ VB.map (\a -> a {age = (age a) + timeStep}) people
		f :: (Person, Float) -> Person
		f (p,r)
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


