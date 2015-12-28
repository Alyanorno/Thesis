{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Prelude hiding (id)
import GHC.Float

import Diagrams.Prelude hiding (start, position)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Grid

import Criterion.Main

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import qualified Data.List as L

import Data.Char (toUpper)
import Data.Int (Int32, Int64)
import Data.Ord
import Data.Maybe (fromJust)

import Control.Monad
import Control.Monad.ST()

import System.Environment
import System.Random
import System.IO

import Stuff
import Definitions
import Birth
import Change
import Death



main :: IO ()
main = do
	args <- liftM parse getArgs :: IO [OptionsType]

	mapM_ doStuff args

	let notFlag (Flag _) = False; notFlag _ = True
	let flags = filter (not.notFlag) args
	unless (any notFlag args) (defaultDoStuff flags)


defaultDoStuff flags = do
	putStrLn "Enter number of iterations: "
	n <- getLine
	let (g, friendss,childrens) = createGeneration (read n)
	let g' = V.filter alive g
	let toProcent x = floor $ (*) 100 $ int2Float x / int2Float (V.length g')
	let toProcent2 x = floor $ (*) 100 $ int2Float x / int2Float (V.length g)
	putStrLn ""
	putStrLn $ "Child: " ++ (show . VB.foldr (+) 0 . VB.map V.length $ childrens)
	putStrLn $ "Total: " ++ (show . V.length $ g)
	putStrLn $ "Alive: " ++ (show . toProcent2 . V.length $ g') ++ "% "
	putStrLn $ "Lover: " ++ (show . toProcent . V.length . V.filter ((/=0) . lover .&&. (<41) . age) $ g') ++ "%"
	putStr "Cult:  "
	VB.mapM_ putStr $ VB.map ((++"% ") . show . toProcent . V.length) $ toBuckets allCulturesVector (cultureToInt . culture) g'
	putStrLn ""
	putStr "Prof:  "
	VB.mapM_ putStr $ VB.map ((++"% ") . show . toProcent . V.length) $ toBuckets allProfessionsVector (professionToInt . profession) g'
	putStrLn ""

	let fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id $ V.head g)))))
	let distanceGraph = calculateDistanceGraph $ fromIDtoGraph friendss
	let numberOf value = VB.sum $ VB.map (V.length . V.filter (==value)) distanceGraph
	when (Flag "path" `elem` flags) (putStrLn $ "Path: " ++ show [numberOf v | v <- [0..10]])

	putStrLn ""
	putStrLn "---------------------"
	putStrLn ""

	when (Flag "loop" `elem` flags) main

doStuff :: OptionsType -> IO ()
doStuff (Pop a b) = genPopulationMap2 a b
doStuff (Cult a b) = genCultureMap a b
doStuff (Prof a b) = genProfessionMap a b
doStuff (Perf outType outFile iter) = withArgs [outType, outFile] . defaultMain . map (\a -> bench (show a) $ nf createGeneration a) $ iter
doStuff (Connections i t st) = renderDiagram name (mapRelations map' r1 r2)
	where
	name = "connections" ++ (toUpper (head st) : tail st) ++ ".svg"
	r1 = selectRelations g (not.comp) friendss (V.head g)
	r2 = selectRelations g comp friendss (V.head g)

	(g, friendss,_) = createGeneration i
	(map',_) = createMaps g

	comp = case t of
		"cult" -> (== stringToCulture st).culture
		"prof" -> (== stringToProfession st).profession
		_ -> error "Second argument for connections must be either \"cult\" or \"prof\""
doStuff (Heatmap name i t st) = renderDiagram name $ populationMapToDiagram $ VB.map V.length $ peopleMap (range*range) $ V.filter comp g'
	where
	comp = case t of
		"cult" -> (== stringToCulture st).culture
		"prof" -> (== stringToProfession st).profession
		_ -> error "Second argument for map must be either \"cult\" or \"prof\""

	(g,_,_) = createGeneration i
	g' = V.filter alive g
	name = "map" ++ (toUpper (head st) : tail st) ++ ".svg"
	range = fromIntegral mapRange
doStuff Help = do
	putStrLn "Thesis Work - Rickard Fridvall"
	putStrLn ""
	putStrLn "pop - [Iteration Start] [Iteration End]"
	putStrLn "cult - [Iteration Start] [Iteration End]"
	putStrLn "prof - [Iteration Start] [Iteration End]"
	putStrLn "perf - [Output Type] [Output File] [Iteration1] [Iteration2] [Iteration3]..."
	putStrLn "connections - [Iteration] [For \"cult\" or \"prof\"] [Subname]"
	putStrLn "map - [Output File] [Iteration] [For \"cult\" or \"prof\"] [Subname]"
	putStrLn "help - No args"
doStuff (Flag _) = return ()

parse (a:ls)
	| a == "pop" = Pop (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "cult" = Cult (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "prof" = Prof (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "perf" = [Perf (ls!!0) (ls!!1) (map read (drop 2 ls))]
	| a == "connections" = Connections (read (ls!!0)) (ls!!1) (ls!!2) : parse (drop 3 ls)
	| a == "map"  = Heatmap (ls!!0) (read (ls!!1)) (ls!!2) (ls!!3) : parse (drop 4 ls)
	| a == "help" = [Help]
	| otherwise = Flag a : parse ls
parse [] = []

data OptionsType = Pop Int Int | Cult Int Int | Prof Int Int | Perf String String [Int] | Connections Int String String | Heatmap String Int String String | Help | Flag String deriving (Eq)






renderDiagram name = renderSVG name dimensions
	where dimensions = mkSizeSpec2D (Just 1000) (Just 1000)

mapRelations :: [[Int]] -> VB.Vector [Int] -> VB.Vector [Int] -> Diagram B
mapRelations p r sr = baseLines <> redLines <> circles
	where
	baseLines = drawAllNodes node p # applyAll (createArrows p r standardArrow) # opacityGroup 0.4
	redLines = drawAllNodes node p # applyAll (createArrows p sr redArrow) # opacityGroup 0.7
	circles = drawAllNodes node p

	drawAllNodes nodeType p = foldr1 (<>) $ zipWith (nodesInCircle nodeType) p list
		where
		list = [r2 (x,y) | x <- [0, spacing..(range-1)*spacing], y <- [0, spacing..(range-1)*spacing]]

		nodesInCircle nodeType p offset
			| length p < 3 = strutX 1 # translate offset
			| otherwise = atPoints (trailVertices $ regPoly (length p) 1 # translate offset) (map nodeType p)


	createArrows p r o = [connectOutside' o from to | temp <- p, from <- temp, to <- r .! from]

	standardArrow = with
		& gaps .~ superTiny
		& arrowHead .~ noHead
		& shaftStyle %~ lc black . opacity 0.01 . lw ultraThin
	redArrow = with
		& gaps .~ superTiny
		& arrowHead .~ noHead
		& shaftStyle %~ lc red . opacity 0.01 . lw ultraThin

	superTiny = normalized 0.002

	spacing = 15
	range = fromIntegral mapRange

	node :: Int -> Diagram B
	node n = circle 0.2 # fc black # lwG 0 # named n

selectRelations g comp friendss start = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss

createMaps g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral mapRange
createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id start)))))

peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * mapRange).position) p, fromID $ id p - id (V.head people))


genPopulationMap2 :: Int -> Int -> IO ()
genPopulationMap2 n1 n2 = mapM_ (\index -> do
		let (g,_,_) = createGeneration index
		let name = "data/populationMap" ++ show index ++ ".svg"
		let range = fromIntegral mapRange

		renderDiagram name $ populationMapToDiagram $ VB.map V.length $ peopleMap (range*range) g
		) [n1..n2]

biggestPopulation = 100 :: Double

populationMapToDiagram :: VB.Vector Int -> Diagram B
populationMapToDiagram population = hsep biggestPopulation [(populationSquares # center) `atop` square (biggestPopulation * fromIntegral mapRange), example]
	where
	populationSquares :: Diagram B
	populationSquares = gridCat $ map ((((sq biggestPopulation :: D V2 Double) # phantom) `atop`) . sq . (\x -> if x > biggestPopulation then biggestPopulation else x) . fromIntegral) $ VB.toList population

	example :: Diagram B
	example = vsep biggestPopulation [sq biggestPopulation ||| f (show $ floor biggestPopulation), sq (biggestPopulation * (3/4)), sq (biggestPopulation / 2), sq (biggestPopulation / 4), hsep biggestPopulation [sq 1, f "1"]]
		where f s = scale biggestPopulation $ text s <> rect 3 1 # lw n

	sq s = square s # fc red # lwG 0
	n = Diagrams.Prelude.none

genPopulationMap n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let (g,_,_) = createGeneration index
	let g' = V.filter alive g
	let positions = let list = V.filter (/=(0,0)) $ V.map position g' in [[((x,y), V.length $ V.filter (\(x',y') -> x == x' && y == y') list) | x <- [0..mapRange]] | y <- [0..mapRange]]
	let f x
		| x == 0 = 0
		| x < 10 = 50
		| x < 50 = 50 + x * 3
		| otherwise = 200
	let toText :: ((Int32, Int32), Int) -> String; toText ((x, y), a) = show x ++ " " ++ show y ++ " " ++ show (255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ show (if a == 0 then 0 else 255) ++ "\n"
	let t = concatMap toText $ concat positions
	writeFile name t
	putStrLn $ "Written: " ++ name)
	$ zip range $ map (\i -> "data/populationMap" ++ show i ++ ".txt") range


genCultureMap n1 n2 = let range = [n1..n2] in mapM_ (\(index, name) -> do
	let f p c
		| V.null p = 0
		| otherwise = float2Int $ int2Float (V.length $ V.filter ((==c).culture) p) / int2Float (V.length p) * 255
	let (g,_,_) = createGeneration index
	let !g' = V.filter alive g
	let !positions = [[((x,y), [f p c | c <- allCultures])  | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
	let t = concatMap (foldr (\((x,y),c) list -> show x ++ " " ++ show y ++ " " ++ setColour c ++ "\n" ++ list) "") positions
	writeFile name t
	putStrLn $ "Written: " ++ name)
	$ zip range $ map (\i -> "data/cultureMap" ++ show i ++ ".txt") range

setColour2 c
	| c !! 2 > 0 = show (c !! 2) ++ " 0 0 255"
	| otherwise = "0 0 0 0"

setColour :: [Int] -> String
setColour c
	| sum c == 0 = "0 0 0 0"
	| b == 0 = "255 0 0 255"
	| b == 1 = "0 255 0 255"
	| b == 2 = "0 0 255 255"
	| otherwise = "0 0 0 0"
	where
		b = snd $ L.maximumBy (comparing fst) $ zip c [0..]


genProfessionMap n1 n2 = let range = [n1..n2] in mapM_ (\(index, name) -> do
	let f :: People -> Int; f l
		| V.null l = 0
		| otherwise = V.foldr (\x list -> list + (*) 100 (professionValue $ profession x)) 0 l `div` V.length l
	let (g,_,_) = createGeneration index
	let !g' = V.filter alive g
	let !positions = [[((x,y), f p) | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
	let t = concatMap (foldr (\((x,y),a) list -> show x ++ " " ++ show y ++ " " ++ show a ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ show (if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions
	writeFile name t
	putStrLn $ "Written: " ++ name)
	$ zip range $ map (\i -> "data/professionMap" ++ show i ++ ".txt") range


calculateDistanceGraph :: VB.Vector (Vector Int) -> VB.Vector (Vector Int)
calculateDistanceGraph graph = VB.generate (VB.length graph) (\i -> V.generate (V.length (graph .! i)) (\l -> pathLength 0 l i))
	where
	pathLength :: Int -> Int -> Int -> Int
	pathLength depth goal current
		| depth == 3 || found = depth
		| otherwise = V.minimum $ V.map (pathLength (depth+1) goal) (graph .! current)
		where found = V.elem goal (graph .! current)

createGeneration :: Int -> (People, Friends, Childrens)
createGeneration n = generations seed 0 n (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	where p = start peopleFromStart

generations :: StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations seed (i+1) n $ (change gen . birth gen . death gen) previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i

