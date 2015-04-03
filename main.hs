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
	putStrLn ""
	print $ V.length $ g
	print $ V.length $ V.filter ((/=0).lover) g
	putStrLn "---------------------"
	main

professions = [(minBound :: Profession)..]
cultures = [(minBound :: Culture)..]

generations :: Int -> Int -> People -> [People]
generations seed i previous = previous : next
	where 
		people = previous
		next = generations seed (i+1) $ ((change gen professions cultures).(birth i gen).death) people
		gen = Xorshift $ fromIntegral $ xor seed i


death :: People -> People
death people = V.map f $ V.filter ((<180).age) people
	where f p
		| (age p) >= 60 = p {alive = False}
		| otherwise = p

seed :: Int
seed = 1 -- If zero the program chrashed on the fourth iteration..... WHAT??!!!


