{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

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
	let g = (generations seed 0 ((start 5))) !! read n
	let g' = V.filter alive g
	putStrLn ""
	print $ V.length g
	print $ V.length g'
	print $ V.length $ V.filter ((/=0).lover) g'
	mapM_ print $ [[V.length $ V.filter (\(x',y') -> x == x' && y == y') (V.map position g') | x <- [0..mapRange]] | y <- [0..mapRange]]
	putStrLn "---------------------"
	print g'
	putStrLn "---------------------"
	main

professions = [(minBound :: Profession)..]
cultures = [(minBound :: Culture)..]

generations :: Int -> Int -> People -> [People]
generations seed i previous = previous : next
	where 
		people = previous
		next = generations seed (i+1) $ ((change gen professions cultures).(birth i gen).(death gen)) people
		gen = Xorshift $ fromIntegral $ xor seed i


death :: RandomGenerator -> People -> People
death gen people = V.map f $ V.zip people' $ fromList $ take (V.length people') (randomRs (0,1) gen :: [Float])
	where
		people' = V.filter ((<150).age) people
		f :: (Person, Float) -> Person
		f (p,r)
			| a < 20 = if r < 10 ^ (log(0.70)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| a < 30 = if r < 10 ^ (log(0.95)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| a < 40 = if r < 10 ^ (log(0.95)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| a < 50 = if r < 10 ^ (log(0.80)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| a < 60 = if r < 10 ^ (log(0.70)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| a < 70 = if r < 10 ^ (log(0.50)*((fromIntegral timeStep)/10)) then p else p {alive = False}
			| otherwise = p {alive = False}
			where a = age p

seed :: Int
seed = 0


