{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Maps where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import Foreign.Storable.Tuple ()

import qualified Data.List as L

import Data.Int (Int32)
import Data.Ord

import Control.Monad.ST()

import System.IO

import Definitions
import Stuff
import Diagram



createMaps g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral $ mapRange

peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * mapRange).position) p, fromID $ (getId p) - (getId $ V.head people))


--genPopulationMap2 :: String -> String -> IO ()
genPopulationMap2 generation n1 n2 = mapM_ (\index -> do
		let (g,_,_) = generation index
		let name = "data/populationMap" ++ (show index) ++ ".svg"
		let range = fromIntegral $ mapRange

		renderDiagram name $ populationMapToDiagram $ VB.map V.length $ peopleMap (range*range) g
		) [read n1..read n2]

genPopulationMap generation n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let (g :: People,_,_) = generation index
	let g' = V.filter alive g
	let positions = let list = V.filter (/=(0,0)) $ V.map position g' in [[((x,y), V.length $ V.filter (\(x',y') -> x == x' && y == y') list) | x <- [0..mapRange]] | y <- [0..mapRange]]
	let f x--let r = x * 4 in if r > 255 then 255 else r
		| x == 0 = 0
		| x < 10 = 50
		| x < 50 = 50 + x * 3
		| otherwise = 200
	let toText :: ((Int32, Int32), Int) -> String; toText ((x, y), a) = (show x) ++ " " ++ (show y) ++ " " ++ (show $ 255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ (if a == 0 then 0 else 255 :: Int)) ++ "\n"
	t <- return $ concat $ map toText $ concat $ positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/populationMap" ++ (show i) ++ ".txt") range


genCultureMap generation n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let f p c
		| V.null p = 0
		| otherwise = float2Int $ (int2Float $ V.length $ V.filter ((==c).culture) p) / (int2Float $ V.length p) * 255
	let (g,_,_) = generation index
	let !g' = V.filter alive g
	let !positions = [[((x,y), [f p c | c <- allCultures])  | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
--	let setColour2 c
--		| c !! 2 > 0 = (show (c !! 2)) ++ " 0 0 255"
--		| otherwise = "0 0 0 0"
	let setColour :: [Int] -> String; setColour c
		| foldr1 (+) c == 0 = "0 0 0 0"
		| b == 0 = "255 0 0 255"
		| b == 1 = "0 255 0 255"
		| b == 2 = "0 0 255 255"
		| otherwise = "0 0 0 0"
		where
			b = snd $ L.maximumBy (comparing fst) $ zip c [0 :: Int ..]
	t <- return $ concat $ map (foldr (\((x,y),c) list -> (show x) ++ " " ++ (show y) ++ " " ++ (setColour c) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/cultureMap" ++ (show i) ++ ".txt") range


genProfessionMap generation n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let f :: People -> Int; f l
		| V.null l = 0
		| otherwise = (V.foldr (\x list -> list + ((*) 100 $ professionValue $ profession x)) 0 l) `div` (V.length l)
	let (g,_,_) = generation index
	let !g' = V.filter alive g
	let !positions = [[((x,y), f p) | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
	t <- return $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ (if a == 0 then 0 else 255 :: Int)) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/professionMap" ++ (show i) ++ ".txt") range

