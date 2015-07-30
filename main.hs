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
import System.Environment
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad
import Control.Monad.ST()
import Control.Concurrent

import System.IO

import Stuff
import Birth
import Change



main = do
	args <- getArgs
	n <- return ""
	n1 <- return ""
	n2 <- return ""

	let doPop = any (=="pop") args
	let doCult = any (=="cult") args
	let doProf = any (=="prof") args

	if doPop || doCult || doProf then do
		putStrLn "Enter number of iterations (from and to): "
		n1 <- getLine
		n2 <- getLine
		when doPop $ genPopulationMap n1 n2
		when doCult $ genCultureMap n1 n2
		when doProf $ genProfessionMap n1 n2
		else do
			putStrLn "Enter number of iterations: "
			n <- getLine
			let p = start peopleFromStart
			let (g,_,_) = generations seed 0 (read n) (p, VB.replicate (VB.length p) V.empty, VB.replicate (VB.length p) V.empty)
			let g' = VB.filter alive g
			let g'' = VB.filter (not.alive) g
			putStrLn ""
			print $ VB.length g'
			print $ VB.length g''
			print $ VB.length $ VB.filter (((/=0).lover) .&&. ((<41).age)) g'
--			print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--			print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g'' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--			print $ (VB.length $ VB.filter ((==female).gender) g', VB.length $ VB.filter ((==male).gender) g')
			print $ VB.map VB.length $ toBuckets allCulturesVector (cultureToInt.culture) g'
			print $ VB.map VB.length $ toBuckets allProfessionsVector (professionToInt.profession) g'

	putStrLn "---------------------"

	when (any (=="loop") args) $ do
		main






genPopulationMap n1 n2 = do
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let p = start peopleFromStart
			let (g,_,_) = generations seed 0 index (p, VB.replicate (VB.length p) V.empty, VB.replicate (VB.length p) V.empty)
			let g' = VB.filter alive g
			let positions = [[((x,y), VB.length $ VB.filter (\(x',y') -> x == x' && y == y') (VB.map position g')) | x <- [0..mapRange]] | y <- [0..mapRange]]
			let f x--let r = x * 4 in if r > 255 then 255 else r
				| x == 0 = 0
				| x < 10 = 50
				| x < 50 = 50 + x * 3
				| otherwise = 200
			hPutStrLn h $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show $ 255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
		$ zip range $ map (\i -> "data/populationMap" ++ (show i) ++ ".txt") range

genCultureMap n1 n2 = do
	let range = [read n1..read n2]
	mapM_ (\(index, name) ->
		withFile name WriteMode (\h -> do
			let p = start peopleFromStart
			let (g,_,_) = generations seed 0 index ((p, VB.replicate (VB.length p) V.empty, VB.replicate (VB.length p) V.empty))
			let g' = VB.filter alive g
			let f p c
				| VB.null p = 0
				| otherwise = float2Int $ (int2Float $ VB.length $ VB.filter ((==c).culture) p) / (int2Float $ VB.length p) * 255
			let positions = [[((x,y), f p endorphi, f p brahmatic, f p uppbrah)  | x <- [0..mapRange], let p = VB.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
			hPutStrLn h $ concat $ map (foldr (\((x,y),r,g,b) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " ++ (show $ if r + g + b == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
		$ zip range $ map (\i -> "data/cultureMap" ++ (show i) ++ ".txt") range

genProfessionMap n1 n2 = do
	let range = [read n1..read n2]
	mapM_ (\(index, name) -> do
		let f l
			| VB.null l = 0
			| otherwise = (VB.foldr (\x list -> list + ((*) 100 $ professionValue x)) 0 (VB.map profession l)) `div` (VB.length l)
		let p = start peopleFromStart
		let (g,_,_) = generations seed 0 index ((p, VB.replicate (VB.length p) V.empty, VB.replicate (VB.length p) V.empty))
		let !g' = VB.filter alive g
		let !positions = [[((x,y), f p) | x <- [0..mapRange], let p = VB.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
		t <- return $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions
		forkIO $ do
			putStrLn $ "Open: " ++ name
			h <- openBinaryFile name WriteMode
			hPutStrLn h t
			putStrLn $ "Close: " ++ name
			hClose h)
		$ zip range $ map (\i -> "data/professionMap" ++ (show i) ++ ".txt") range






generations :: StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations seed (i+1) n $ ((change gen).(birth gen).(death gen)) previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i


death :: RandomGenerator -> (People, Friends, Childrens) -> (People, Friends, Childrens)
death gen (people, friends, childrens) = (VB.map f $ VB.zip people' $ r, friends', childrens')
	where
		friends' = if VB.length friends - VB.length people' > 0 then VB.take (VB.length people') friends else friends
		childrens' = if VB.length childrens - VB.length people' > 0 then VB.take (VB.length people') childrens else childrens

		r = VB.fromList $ map double2Float $ take (VB.length people') $ f $ pureMT $ fromIntegral $ fromXorshift gen
			where f g = let (v,g') = R.randomDouble g in v : f g'

		(_,people') = VB.break ((<80).age) $ VB.map (\a -> a {age = (age a) + (fromIntegral timeStep)}) people
		f :: (Person, Float) -> Person
		f (p,r)
			| a < 20 = if r < 0.97 then p else p {dead = 1} 
			| a < 30 = if r < 0.99 then p else p {dead = 1}
			| a < 40 = if r < 0.99 then p else p {dead = 1}
			| a < 50 = if r < 0.98 then p else p {dead = 1}
			| a < 60 = if r < 0.97 then p else p {dead = 1}
			| a < 70 = if r < 0.95 then p else p {dead = 1}
			| a < 80 = if r < 0.90 then p else p {dead = 1}
--			| otherwise = p {dead = 1}
			where a = age p
{-			| a < 20 = if r < 0.97 .^ timeStep' then p else p {alive = False} 
			| a < 30 = if r < 0.99 .^ timeStep' then p else p {alive = False}
			| a < 40 = if r < 0.99 .^ timeStep' then p else p {alive = False}
			| a < 50 = if r < 0.98 .^ timeStep' then p else p {alive = False}
			| a < 60 = if r < 0.97 .^ timeStep' then p else p {alive = False}
			| a < 70 = if r < 0.95 .^ timeStep' then p else p {alive = False}
			| otherwise = p {alive = False}
			where a = age p; (.^) a b = (**) a b -}
		timeStep' :: Float
		timeStep' = fromIntegral timeStep

seed :: StdGen
seed = mkStdGen 0


