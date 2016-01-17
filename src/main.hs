{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
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
import Data.Maybe (fromJust)
import Data.String.Utils
import Data.Maybe
import Data.List (unfoldr, tails, find)
import Data.List.Split (chunksOf)

import Control.Monad
import Control.Monad.ST()

import System.Environment
import System.Random hiding (split)
import System.IO

import GHC.Float (float2Double)

import Stuff
import Definitions
import Birth
import Change
import Death




main :: IO ()
main = do
	args <- liftM parse getArgs :: IO [OptionsType]
	let f (OptionsFile _) = True; f _ = False
	(options, terrainMap) <- maybe (return $ parseOptions []) (\(OptionsFile a) -> liftM (parseOptions.lines) $ readFile a) $ find f args
	options <- liftM (\lines -> options {staticTerrainMap = V.fromList . map (\a -> case a of 1 -> 10000; 2 -> impossiblePos; _ -> a) . readAll $ lines}) . readFile $ terrainMap

	mapM_ (doStuff options) args

	let notFlag (Flag _) = False; notFlag (OptionsFile _) = False; notFlag _ = True
	let isFlag (Flag _) = True; isFlag _ = False
	let flags = filter isFlag args
	unless (any notFlag args) (defaultDoStuff options flags)


parseOptions :: [String] -> (Options, String)
parseOptions = foldr f start.map (span (/='=')).filter (not.null .&&. (/="\n") .&&. (/="--").head.words.lstrip)
	where
	f :: (String, String) -> (Options, String) -> (Options, String)
	f (l,r) (opt, terrainMap) = case strip l of
		"mapRange" -> (opt {mapRange = read r'}, terrainMap)
		"timeStep" -> (opt {timeStep = read r'}, terrainMap)
		"peopleFromStart" -> (opt {peopleFromStart = read r'}, terrainMap)
		"moveRange" -> (opt {moveRange = read r'}, terrainMap)
		"positionSampling" -> (opt {positionSampling = read r'}, terrainMap)
		"startPosition" -> (opt {startPosition = (\[a,b] -> (a,b)) $ readAll r'}, terrainMap)
		"scaleDistanceFromCenter" -> (opt {scaleDistanceFromCenter = (\a -> 100 * (read r') - a * (read r'))}, terrainMap)
		"scaleDistanceFromCulturalCenter" -> (opt {scaleDistanceFromCulturalCenter = (*) (read r')}, terrainMap)
		"scaleConcentrationOfPeople" -> (opt {scaleConcentrationOfPeople = (\a -> let t = negate a ** (r''!!1) in if t < (r''!!0) then (r''!!0) {-impossiblePos-} else t )}, terrainMap)
		"scaleCulturalMap" -> (opt {scaleCulturalMap = (*) (read r')}, terrainMap)
		"scaleProfessionalMap" -> (opt {scaleProfessionalMap = (*) (read r')}, terrainMap)
		"staticTerrainMap" -> (opt, strip r')
		"staticProfessionalRelations" -> (opt {staticProfessionalRelations = VB.fromList $ map (V.fromList) $ chunksOf (round $ sqrt $ fromIntegral $ length r'') r''}, terrainMap)
		"professionValue" -> (opt {professionValue = ((r'') !!).professionToInt}, terrainMap)
		"seed" -> (opt {seed = mkStdGen $ read r'}, terrainMap)
		_ -> error $ "Unknown option: " ++ strip l
		where
		r' = head $ split "--" $ tail r
		r'' = readAll r'

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
	let (g, friendss,childrens) = createGeneration opt (read n)
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


--	let i2f :: Int -> Float; i2f = fromIntegral
--	let numberOf' value = (\a -> (VB.sum a) / (i2f $ VB.length a)) $ VB.map (\a -> (\b -> b / (i2f $ V.length a)) . i2f . V.length . V.filter (<value) $ a) distanceGraph
--	print $ numberOf' 4

--	print $ VB.length friendss
--	writeFile "dataRelationsSizes.txt" $ VB.foldr1 (\a b -> a++"\n"++b) $ VB.imap (\i a -> ((++) ((show i)++" ")) . show . V.length $ a) friendss
--	writeFile "dataRelationsEdges.txt" $ VB.foldr1 (\a b -> a++"\n"++b) $ VB.imap (\i a -> ((++) ((show i)++" ")) . L.foldr1 (\a b -> a++"\n"++(show i)++" "++b) . L.map (show . (\x -> x - fromID (id $ V.head g)) . fromID) . V.toList $ a) $ friendss

	when (Flag "loop" `elem` flags) main

doStuff :: Options -> OptionsType -> IO ()
doStuff opt (Pop a b) = genPopulationMap opt a b
doStuff opt (Cult a b) = genCultureMap opt a b
doStuff opt (Prof a b) = genProfessionMap opt a b
doStuff opt (Perf outType outFile iter) = withArgs [outType, outFile] . defaultMain . map (\a -> bench (show a) $ nf (createGeneration opt) a) $ iter
doStuff opt (Connections i t st) = renderDiagram name (mapRelations (mapRange opt) map' r1 r2)
	where
	name = "connections" ++ (toUpper (head st) : tail st) ++ ".svg"
	r1 = selectRelations g (not.comp) friendss (V.head g)
	r2 = selectRelations g comp friendss (V.head g)

	(g, friendss,_) = createGeneration opt i
	(map',_) = createMaps (mapRange opt) g

	comp = case t of
		"cult" -> (== stringToCulture st).culture
		"prof" -> (== stringToProfession st).profession
		_ -> error "Second argument for connections must be either \"cult\" or \"prof\""
doStuff opt (Heatmap i t st) = renderDiagram name $ populationMapToDiagram (fromIntegral $ mapRange opt) $ VB.map V.length $ peopleMap (range*range) $ V.filter comp g'
	where
	comp = case t of
		"cult" -> (== stringToCulture st).culture
		"prof" -> (== stringToProfession st).profession
		_ -> error "Second argument for map must be either \"cult\" or \"prof\""

	(g,_,_) = createGeneration opt i
	g' = V.filter alive g
	name = "data/map" ++ (toUpper (head st) : tail st) ++ ".svg"
	range = fromIntegral $ mapRange opt
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

parse (a:ls)
	| a == "pop" = Pop (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "cult" = Cult (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "prof" = Prof (read (ls!!0)) (read (ls!!1)) : parse (drop 2 ls)
	| a == "perf" = [Perf ("--" ++ (ls!!0)) ("--" ++ (ls!!1)) (map read (drop 2 ls))]
	| a == "connections" = Connections (read (ls!!0)) (ls!!1) (ls!!2) : parse (drop 3 ls)
	| a == "map"  = Heatmap (read (ls!!0)) (ls!!1) (ls!!2) : parse (drop 4 ls)
	| a == "optionsFile" = OptionsFile (ls!!0) : parse (drop 1 ls)
	| a == "help" = [Help]
	| otherwise = Flag a : parse ls
parse [] = []

data OptionsType = Pop Int Int | Cult Int Int | Prof Int Int | Perf String String [Int] | Connections Int String String | Heatmap Int String String | OptionsFile String | Help | Flag String deriving (Eq)






renderDiagram name = renderSVG name dimensions
	where dimensions = mkSizeSpec2D (Just 1000) (Just 1000)

mapRelations :: Int32 -> [[Int]] -> VB.Vector [Int] -> VB.Vector [Int] -> Diagram B
mapRelations mapRange p r sr = baseLines <> redLines <> circles
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
	range = fromIntegral $ mapRange

	node :: Int -> Diagram B
	node n = circle 0.2 # fc black # lwG 0 # named n

selectRelations g comp friendss start = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss

createMaps mapRange g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral mapRange
createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id start)))))

peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * (round $ sqrt $ fromIntegral size)).position) p, fromID $ id p - id (V.head people))


genPopulationMap :: Options -> Int -> Int -> IO ()
genPopulationMap opt n1 n2 = mapM_ (\index -> do
		let (g,_,_) = createGeneration opt index
		let name = "data/populationMap" ++ show index ++ ".svg"
		let range = fromIntegral $ mapRange opt

		renderDiagram name $ populationMapToDiagram (fromIntegral $ mapRange opt) $ VB.map V.length $ peopleMap (range*range) g
		) [n1..n2]

biggestPopulation = 50 :: Double

populationMapToDiagram :: Double -> VB.Vector Int -> Diagram B
populationMapToDiagram range population = hsep biggestPopulation [(populationSquares # center) `atop` square (biggestPopulation * range), example]
	where
	populationSquares :: Diagram B
	populationSquares = gridCat $ map ((((sq biggestPopulation :: D V2 Double) # phantom) `atop`) . sq . (\x -> if x > biggestPopulation then biggestPopulation else x) . fromIntegral) $ VB.toList population

	example :: Diagram B
	example = vsep biggestPopulation [sq biggestPopulation ||| f (show $ floor biggestPopulation), sq (biggestPopulation * (3/4)), sq (biggestPopulation / 2), sq (biggestPopulation / 4), hsep biggestPopulation [sq 1, f "1"]]
		where f s = scale biggestPopulation $ text s <> rect 3 1 # lw n

	sq s = square s # fc red # lwG 0
	n = Diagrams.Prelude.none

genCultureMap opt n1 n2 = let range = [n1..n2] in mapM_ (\(index, name) -> do
	let f p c
		| V.null p = 0
		| otherwise = float2Int $ int2Float (V.length $ V.filter ((==c).culture) p) / int2Float (V.length p) * 255
	let (g,_,_) = createGeneration opt index
	let !g' = V.filter alive g
	let !positions = concat [[[f p c | c <- allCultures] | x <- [0..mapRange opt], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange opt]]
	let sq (s, c) = square s # fc c # lwG 0
	renderDiagram name $ gridCat $ map (sq . setColour) positions
	putStrLn $ "Written: " ++ name)
	$ zip range $ map (\i -> "data/cultureMap" ++ show i ++ ".svg") range

setColour :: (Ord a, Floating a) => [Int] -> (Double, Colour a)
setColour c
	| sum c == 0 = (0, white) --"0 0 0 0"
	| b == 0 = (1, red) --"255 0 0 255"
	| b == 1 = (1, green) --"0 255 0 255"
	| b == 2 = (1, blue) --"0 0 255 255"
	| otherwise = (0, white) --"0 0 0 0"
	where b = snd $ L.maximumBy (comparing fst) $ zip c [0 :: Int ..]

genProfessionMap opt n1 n2 = let range = [n1..n2] in mapM_ (\(index, name) -> do
	let f :: People -> Float; f l
		| V.null l = 0
		| otherwise = let t = V.foldr1 (+) (V.map ((professionValue opt) . profession) l) / (fromIntegral $ V.length l); m = 1.5 in (if t > 2.5 then 2.5 else (if t < m then m else t)) - m

	let (g,_,_) = createGeneration opt index
	let !g' = V.filter alive g
	let !positions = concat [[f p | x <- [0..mapRange opt], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange opt]]
	let sq s = square s # fc red # lwG 0
	renderDiagram name $ gridCat $ map sq positions
	putStrLn $ "Written: " ++ name)
	$ zip range $ map (\i -> "data/professionMap" ++ show i ++ ".svg") range


calculateDistanceGraph :: VB.Vector (Vector Int) -> VB.Vector (Vector Int)
calculateDistanceGraph graph = VB.generate (VB.length graph) (\i -> V.generate (V.length (graph .! i)) (\l -> pathLength 0 l i))
	where
	pathLength :: Int -> Int -> Int -> Int
	pathLength depth goal current
		| depth == 4 || found = depth
		| otherwise = V.minimum $ V.map (pathLength (depth+1) goal) (graph .! current)
		where found = V.elem goal (graph .! current)

createGeneration :: Options -> Int -> (People, Friends, Childrens)
createGeneration opt n = generations opt (seed opt) 0 n (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	where p = start (mapRange opt) (startPosition opt) (peopleFromStart opt)

generations :: Options -> StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations opt seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations opt seed (i+1) n $ change opt gen . birth opt gen . death opt gen $ previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i

