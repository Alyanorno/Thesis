{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Prelude hiding (id)
import GHC.Float

import Diagrams.Prelude hiding (start, position, Options)
import Diagrams.Backend.SVG hiding (Options)
import Diagrams.TwoD.Layout.Grid

import Criterion.Main

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import qualified Data.List as L

import Data.Char (toUpper)
import Data.Int (Int32, Int64)
import Data.Ord
import Data.String.Utils
import Data.Maybe
import Data.List (unfoldr, tails, find)
import Data.List.Split (chunksOf)

import Control.Monad
import Control.Monad.ST()

import System.Environment
import System.Random hiding (split)
import System.IO

import Stuff
import Definitions
import Birth
import Change
import Death
import Renderings



main :: IO ()
main = do
	args <- liftM parse getArgs :: IO [OptionsType]

	let f (OptionsFile _) = True; f _ = False
	let readOptions (OptionsFile a) = liftM (parseOptions.lines) $ readFile a
	(options, terrainMap) <- maybe (return $ parseOptions []) readOptions $ find f args

	let convertTerrainValues = V.fromList . map (\a -> case a of 1 -> 10000; 2 -> impossiblePos; _ -> a)
	let parseOptionsFile lines = options {staticTerrainMap = convertTerrainValues . readAll $ lines}
	options <- liftM parseOptionsFile . readFile $ terrainMap

	mapM_ (doStuff options) args

	let notFlag (Flag _) = False; notFlag (OptionsFile _) = False; notFlag _ = True
	let isFlag (Flag _) = True; isFlag _ = False
	let flags = filter isFlag args
	unless (any notFlag args) (defaultDoStuff options flags)


parseOptions :: [String] -> (Options, String)
parseOptions = foldr (f . span (/= '=')) start . filter (not.null .&&. (/="\n") .&&. (/="--").head.words.lstrip)
	where
	f :: (String, String) -> (Options, String) -> (Options, String)
	f (l,r) (opt, terrainMap) = case strip l of
		"mapRange" ->
			(opt {mapRange = read r'}, terrainMap)
		"timeStep" ->
			(opt {timeStep = read r'}, terrainMap)
		"peopleFromStart" ->
			(opt {peopleFromStart = read r'}, terrainMap)
		"moveRange" ->
			(opt {moveRange = read r'}, terrainMap)
		"positionSampling" ->
			(opt {positionSampling = read r'}, terrainMap)
		"startPosition" ->
			(opt {startPosition = (\[a,b] -> (a,b)) $ readAll r'}, terrainMap)
		"scaleDistanceFromCenter" ->
			(opt {scaleDistanceFromCenter = \a -> 100 * read r' - a * read r'}, terrainMap)
		"scaleDistanceFromCulturalCenter" ->
			(opt {scaleDistanceFromCulturalCenter = (*) (read r')}, terrainMap)
		"scaleConcentrationOfPeople" ->
			(opt {scaleConcentrationOfPeople = \a -> (\[r1,r2] -> max (0 - a ** r2) r1) r''}, terrainMap)
		"scaleCulturalMap" ->
			(opt {scaleCulturalMap = (*) (read r')}, terrainMap)
		"scaleProfessionalMap" ->
			(opt {scaleProfessionalMap = (*) (read r')}, terrainMap)
		"staticTerrainMap" ->
			(opt, strip r')
		"staticProfessionalRelations" ->
			(opt {staticProfessionalRelations = readProf r''}, terrainMap)
		"professionValue" ->
			(opt {professionValue = (r'' !!).professionToInt}, terrainMap)
		"seed" ->
			(opt {seed = mkStdGen $ read r'}, terrainMap)
		_ -> error $ "Unknown option: " ++ strip l
		where
		r' = head $ split "--" $ tail r
		r'' = readAll r'

		readProf a = VB.fromList . map V.fromList . chunksOf (round . sqrt . fromIntegral $ length a) $ a


	start = (Options
		(50 :: Int32)
		(4 :: Int)
		(50 :: Int)
		(3 :: Int32)
		(10 :: Int)
		((25,25) :: (Int32, Int32))
		((\a -> 100 - a * 1) :: Float -> Float)
		((* negate 0.1) :: Float -> Float)
		((\a -> let t = negate a ** 1.2 in if t < negate 50000 then negate 50000 else t) :: Float -> Float)
		((* 1) :: Float -> Float)
		((* 1) :: Float -> Float)
		V.empty
		(VB.fromList $ map V.fromList [[1,1,0,0],[1,1,0,0],[0,0,1,1],[0,0,1,1]])
		(([1,2,0,0] !!).professionToInt)
		(mkStdGen 0),
		"valuesTerrain.txt")

readAll :: (Num a, Read a) => String -> [a]
readAll = unfoldr (listToMaybe.concatMap reads.tails)


defaultDoStuff opt flags = do
	putStrLn "Enter number of iterations: "
	n <- getLine
	let (g, friendss) = createGeneration opt (read n)
	let g' = V.filter alive g
	let toProcent x = floor $ (*) 100 $ int2Float x / int2Float (V.length g')
	let toProcent2 x = floor $ (*) 100 $ int2Float x / int2Float (V.length g)
	putStrLn ""
	putStrLn $ "Total: " ++ (show . V.length $ g)
	putStrLn $ "Alive: " ++ (show . toProcent2 . V.length $ g') ++ "% "
	putStrLn $ "Lover: " ++ (show . toProcent . V.length . V.filter ((/=0) . lover .&&. (<41) . age) $ g') ++ "%"
	putStr "Cult:  "
	let printLengthInPercent = VB.mapM_ putStr . VB.map ((++"% ") . show . toProcent . V.length)
	printLengthInPercent $ toBuckets allCulturesVector (cultureToInt . culture) g'
	putStrLn ""
	putStr "Prof:  "
	printLengthInPercent $ toBuckets allProfessionsVector (professionToInt . profession) g'
	putStrLn ""

	let fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id $ V.head g)))))
	let distanceGraph = calculateDistanceGraph $ fromIDtoGraph friendss
	let numberOf value = VB.sum $ VB.map (V.length . V.filter (==value)) distanceGraph
	when (Flag "path" `elem` flags) (putStrLn $ "Path: " ++ show [numberOf v | v <- [0..10]])

	putStrLn ""
	putStrLn "---------------------"
	putStrLn ""

	when (Flag "loop" `elem` flags) main

doStuff :: Options -> OptionsType -> IO ()
doStuff opt (Pop a b) = writeDiagramMap opt a b "populationMap" renderPopulationMap
doStuff opt (Cult a b) = writeDiagramMap opt a b "cultureMap" renderCultureMap
doStuff opt (Prof a b) = writeDiagramMap opt a b "professionMap" renderProfessionMap
doStuff opt (Mega a b) = writeDiagramMap opt a b "megaMap" renderMegaMap
doStuff opt (Iter a) = (print $ V.length $ fst $ createGeneration opt a) >> return ()
doStuff opt (Perf outType outFile iter) = out . map (\a -> bench (show a) $ nf (createGeneration opt) a) $ iter
	where out = withArgs [outType, outFile] . defaultMain
doStuff opt (Connections i t st) = renderConnections t st opt g friendss
	where
	(g, friendss) = createGeneration opt i
doStuff opt (Heatmap i t st) = renderHeatmap t st opt (createGeneration opt i)
doStuff _ (OptionsFile _) = return ()
doStuff _ Help = do
	putStrLn "Thesis Work - Rickard Fridvall"
	putStrLn ""
	putStrLn "pop - [Iteration Start] [Iteration End]"
	putStrLn "cult - [Iteration Start] [Iteration End]"
	putStrLn "prof - [Iteration Start] [Iteration End]"
	putStrLn "perf - [Output Type] [Output File] [Iteration1] [Iteration2] [Iteration3]..."
	putStrLn "connections - [Iteration] [For \"cult\" or \"prof\"] [Subname]"
	putStrLn "map - [Iteration] [For \"cult\" or \"prof\"] [Subname]"
	putStrLn "help - No args"
	putStrLn ""
	putStrLn "Flags:"
	putStrLn "    loop"
	putStrLn "    path"
doStuff _ (Flag _) = return ()

parse [] = []
parse (a:ls)
	| a == "pop" = (\(a1:a2:ls) -> Pop (read a1) (read a2) : parse ls) ls
	| a == "cult" = (\(a1:a2:ls) -> Cult (read a1) (read a2) : parse ls) ls
	| a == "prof" = (\(a1:a2:ls) -> Prof (read a1) (read a2) : parse ls) ls
	| a == "mega"  = (\(a1:a2:ls) -> Mega (read a1) (read a2) : parse ls) ls
	| a == "iterations" = (\(a1:ls) -> Iter (read a1) : parse ls) ls
	| a == "perf" = (\(a1:a2:ls) -> [Perf ("--" ++ a1) a2 (map read ls)]) ls
	| a == "connections" = (\(a1:a2:a3:ls) -> Connections (read a1) a2 a3 : parse ls) ls
	| a == "map"  = (\(a1:a2:a3:ls) -> Heatmap (read a1) a2 a3 : parse ls) ls
	| a == "optionsFile" = (\(a1:ls) -> OptionsFile a1 : parse ls) ls
	| a == "help" = [Help]
	| otherwise = Flag a : parse ls

data OptionsType =
	Pop Int Int |
	Cult Int Int |
	Prof Int Int |
	Mega Int Int |
	Iter Int |
	Perf String String [Int] |
	Connections Int String String |
	Heatmap Int String String |
	OptionsFile String |
	Help |
	Flag String deriving (Eq)


writeDiagramMap opt n1 n2 name fun = let range = [n1..n2] in mapM_ (\(index, name') -> do
	renderDiagram name' $ fun opt (fst $ createGeneration opt index)
	putStrLn $ "Written: " ++ name')
	$ zip range $ map (\i -> "data/" ++ name ++ show i ++ ".svg") range

calculateDistanceGraph :: VB.Vector (Vector Int) -> VB.Vector (Vector Int)
calculateDistanceGraph graph = VB.generate (VB.length graph) f
	where
	f i = V.generate (V.length (graph .! i)) (\l -> pathLength 0 l i)

	pathLength :: Int -> Int -> Int -> Int
	pathLength depth goal current
		| depth == 4 || found = depth
		| otherwise = V.minimum $ V.map (pathLength (depth+1) goal) (graph .! current)
		where found = V.elem goal (graph .! current)

createGeneration :: Options -> Int -> (People, Friends)
createGeneration opt n = generations opt (seed opt) 0 n (p, VB.replicate (V.length p) V.empty)
	where p = start (startPosition opt) (peopleFromStart opt)

generations :: Options -> StdGen -> Int -> Int -> (People, Friends) -> (People, Friends)
generations opt seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations opt seed (i+1) n $ change opt gen . birth opt gen . death opt gen $ previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i

