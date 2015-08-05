{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import qualified Data.List as L
import Data.List.Split (chunksOf)

import Data.Ord
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
			let (g,_,_) = generations seed 0 (read n) (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
			let g' = V.filter alive g
			let g'' = V.filter (not.alive) g
			let toProcent x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g')
			let toProcent2 x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g)
			putStrLn ""
			putStrLn $ "Total: " ++ (show $ V.length g)
			putStrLn $ "Alive: " ++ (show $ toProcent2 $ V.length g') ++ "% "
			putStrLn $ "Lover: " ++ (show $ toProcent $ V.length $ V.filter (((/=0).lover) .&&. ((<41).age)) g') ++ "%"
--			print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--			print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g'' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--			print $ (VB.length $ VB.filter ((==female).gender) g', VB.length $ VB.filter ((==male).gender) g')
			putStr "Cult:  "
			VB.mapM_ putStr $ VB.map ((++"% ").show.toProcent.V.length) $ toBuckets allCulturesVector (cultureToInt.culture) g'
			putStrLn ""
			putStr "Prof:  "
			VB.mapM_ putStr $ VB.map ((++"% ").show.toProcent.V.length) $ toBuckets allProfessionsVector (professionToInt.profession) g'
			putStrLn ""

	putStrLn "---------------------"
	putStrLn ""

	when (any (=="loop") args) $ do
		main






genPopulationMap n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let p = start peopleFromStart
	let (g,_,_) = generations seed 0 index (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	let !g' = V.filter alive g
	let !positions = [[((x,y), V.length $ V.filter (\(x',y') -> x == x' && y == y') (V.map position g')) | x <- [0..mapRange]] | y <- [0..mapRange]]
	let f x--let r = x * 4 in if r > 255 then 255 else r
		| x == 0 = 0
		| x < 10 = 50
		| x < 50 = 50 + x * 3
		| otherwise = 200
	t <- return $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show $ 255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/populationMap" ++ (show i) ++ ".txt") range

genCultureMap n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let f p c
		| V.null p = 0
		| otherwise = float2Int $ (int2Float $ V.length $ V.filter ((==c).culture) p) / (int2Float $ V.length p) * 255
	let p = start peopleFromStart
	let (g,_,_) = generations seed 0 index ((p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty))
	let !g' = V.filter alive g
	let !positions = [[((x,y), [f p c | c <- allCultures])  | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
--	hPutStrLn h $ concat $ map (foldr (\((x,y),r,g,b) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " ++ (show $ if r + g + b == 0 then 0 else 255) ++ "\n" ++ list) "") positions))
	t <- return $ concat $ map (foldr (\((x,y),c) list -> (show x) ++ " " ++ (show y) ++ " " ++ (setColour c) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/cultureMap" ++ (show i) ++ ".txt") range

setColour2 c
	| c !! 2 > 0 = (show (c !! 2)) ++ " 0 0 255"
	| otherwise = "0 0 0 0"

setColour :: [Int] -> String
setColour c
	| foldr1 (+) c == 0 = "0 0 0 0"
	| b == 0 = "255 0 0 255"
	| b == 1 = "0 255 0 255"
	| b == 2 = "0 0 255 255"
	| otherwise = "0 0 0 0"
	where
		b = snd $ L.maximumBy (comparing fst) $ zip c [0..]


genProfessionMap n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let f :: People -> Int; f l
		| V.null l = 0
		| otherwise = (V.foldr (\x list -> list + ((*) 100 $ professionValue $ profession x)) 0 l) `div` (V.length l)
	let p = start peopleFromStart
	let (g,_,_) = generations seed 0 index ((p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty))
	let !g' = V.filter alive g
	let !positions = [[((x,y), f p) | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
	t <- return $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
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
death gen (people, friends, childrens) = (V.zipWith f people' r, friends', childrens')
	where
		friends' = if VB.length friends - V.length people' > 0 then VB.take (V.length people') friends else friends
		childrens' = if VB.length childrens - V.length people' > 0 then VB.take (V.length people') childrens else childrens

		r = V.fromList $ map double2Float $ take (V.length people') $ f $ pureMT $ fromIntegral $ fromXorshift gen
			where f g = let (v,g') = R.randomDouble g in v : f g'

		(_,people') = V.break ((<80).age) $ V.map (\a -> a {age = (age a) + (fromIntegral timeStep)}) people
		f :: Person -> Float -> Person
		f p r
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


