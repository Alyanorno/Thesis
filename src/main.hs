{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Prelude hiding (id)
import GHC.Float

import Criterion.Main

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import qualified Data.List as L

import Data.Char (toUpper)
import Data.Int (Int64)
import Data.Maybe (fromJust)

import Control.Monad
import Control.Monad.ST()

import System.Environment
import System.Random

import Definitions
import Stuff
import Birth
import Death
import Change
import Diagram
import Maps


main :: IO ()
main = do
	args <- getArgs

	let doPop = any (=="pop") args
	let doCult = any (=="cult") args
	let doProf = any (=="prof") args

	if doPop || doCult || doProf then do
		putStrLn "Enter number of iterations (from and to): "
		n1 <- getLine
		n2 <- getLine
		when doPop $ genPopulationMap2 createGeneration n1 n2
		when doCult $ genCultureMap createGeneration n1 n2
		when doProf $ genProfessionMap createGeneration n1 n2
	else if any (=="perf") args then do
		putStrLn "Enter iterations to test (list): "
		n <- getLine
		let options = ["--csv", "mainPerf.csv"]
--		let options = ["--output", "mainPerf.html"]
		withArgs options $ defaultMain [bench a $ nf createGeneration (read a) | a <- words n]
	else if any (=="connections") args then do
		putStrLn "Enter number of iterations: "
		n <- getLine
		putStrLn ""
		let (g, friendss,_) = createGeneration (read n)
		let (map',_) = createMaps g

		let index = fromJust $ L.findIndex (=="connections") args
		let comp = case args !! (index + 1) of
			"culty" -> (==(stringToCulture $ args !! (index + 2))).culture
			"profy" -> (==(stringToProfession $ args !! (index + 2))).profession
			_ -> error "Second argument for connections must be either \"culty\" or \"profy\""

		let name = "connections" ++ (let a = args !! (index + 2) in (toUpper $ head a) : (tail a)) ++ ".png"
		let r1 = selectRelations g (not.comp) friendss (V.head g)
		let r2 = selectRelations g comp friendss (V.head g)

		putStrLn "Starting connections rendering"
		renderDiagram name (mapRelations map' r1 r2)
		putStrLn "Completed connections rendering"
	else if any (=="map") args then do
		putStrLn "Enter number of iterations: "
		n <- getLine
		putStrLn ""

		let index = fromJust $ L.findIndex (=="map") args
		let comp = case args !! (index + 1) of
			"culty" -> (==(stringToCulture $ args !! (index + 2))).culture
			"profy" -> (==(stringToProfession $ args !! (index + 2))).profession
			_ -> error "Second argument for map must be either \"culty\" or \"profy\""

		let (g,_,_) = createGeneration (read n)
		let g' = V.filter alive g
		let name = "map" ++ (let a = args !! (index + 2) in (toUpper $ head a) : (tail a)) ++ ".svg"
		let range = fromIntegral $ mapRange

		putStrLn "Starting map rendering"
		renderDiagram name $ populationMapToDiagram $ VB.map V.length $ peopleMap (range*range) $ V.filter comp g'
		putStrLn "Completed map rendering"
	else do
		putStrLn "Enter number of iterations: "
		n <- getLine
		let (g, friendss,childrens) = createGeneration (read n)
		let g' = V.filter alive g
		let toProcent x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g') :: Integer -- If Int, the compiler gets stackoverflow
		let toProcent2 x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g) :: Integer -- If Int, the compiler gets stackoverflow
		putStrLn ""
		putStrLn $ "Child: " ++ (show . VB.foldr (+) 0 . VB.map V.length $ childrens)
		putStrLn $ "Total: " ++ (show . V.length $ g)
		putStrLn $ "Alive: " ++ (show . toProcent2 . V.length $ g') ++ "% "
		putStrLn $ "Lover: " ++ (show . toProcent . V.length . V.filter ((/=0) . lover .&&. (<41) . age) $ g') ++ "%"
--		print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--		print $ [VB.length $ VB.filter (((min<=) .&&. (<max)).age) g'' | let ages = [0,4..80] ++ [130], (min,max) <- zip ages (tail ages)]
--		print $ (VB.length $ VB.filter ((==female).gender) g', VB.length $ VB.filter ((==male).gender) g')
		putStr "Cult:  "
		VB.mapM_ putStr $ VB.map ((++"% ") . show . toProcent . V.length) $ toBuckets allCulturesVector (cultureToInt . culture) g'
		putStrLn ""
		putStr "Prof:  "
		VB.mapM_ putStr $ VB.map ((++"% ") . show . toProcent . V.length) $ toBuckets allProfessionsVector (professionToInt . profession) g'
		putStrLn ""

		let fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(getId $ V.head g)))))
		let distanceGraph = calculateDistanceGraph $ fromIDtoGraph friendss
		let numberOf value = VB.sum $ VB.map (V.length . V.filter (==value)) distanceGraph
		when (any (=="path") args) (putStrLn $ "Path: " ++ (show [numberOf v | v <- [0..10]]))

		putStrLn ""

	putStrLn "---------------------"
	putStrLn ""

	when (any (=="loop") args) $ do
		main



selectRelations g comp friendss start = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss

createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map ((V.filter (>=0)).(V.map (fromID.(+(-(getId start))))))

calculateDistanceGraph :: VB.Vector (Vector Int) -> VB.Vector (Vector Int)
calculateDistanceGraph graph = VB.generate (VB.length graph) (\i -> V.generate (V.length (graph .! i)) (\l -> pathLength 0 l i))
	where
	pathLength :: Int -> Int -> Int -> Int
	pathLength depth goal current
		| depth == 3 || found = depth
		| otherwise = V.minimum $ V.map (pathLength (depth+1) goal) (graph .! current)
		where found = V.elem goal (graph .! current)

createGeneration n = generations seed 0 n (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	where p = startPopulation peopleFromStart

generations :: StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations seed' i n previous
	| i >= n = previous
	| otherwise = next'
	where 
		next' = generations seed' (i+1) n $ ((change gen).(birth gen).(death gen)) previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed' :: [Int64]) !! i

