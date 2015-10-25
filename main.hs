{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

import Prelude hiding (id)
import GHC.Float

import Diagrams.Prelude hiding (start, position)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Size

import Criterion.Main

import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L

import qualified Data.Foldable as F
import Data.Char (toUpper)
import Data.Int (Int32, Int64)
import Data.Ord
import Data.String (words)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)

import Control.Monad
import Control.Monad.ST()
import Control.Concurrent

import System.Environment
import System.Random
import System.Random.Mersenne.Pure64 as R
import System.IO
import System.Environment (withArgs)

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
		when doPop $ genPopulationMap2 n1 n2
		when doCult $ genCultureMap n1 n2
		when doProf $ genProfessionMap n1 n2
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
		let (map', mapLengths') = createMaps g

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
		let g'' = V.filter (not.alive) g
		let toProcent x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g')
		let toProcent2 x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g)
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

		let fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id $ V.head g)))))
		let distanceGraph = calculateDistanceGraph $ fromIDtoGraph friendss
		let numberOf value = VB.sum $ VB.map (V.length . V.filter (==value)) distanceGraph
		when (any (=="path") args) (putStrLn $ "Path: " ++ (show [numberOf v | v <- [0..10]]))

		putStrLn ""

	putStrLn "---------------------"
	putStrLn ""

	when (any (=="loop") args) $ do
		main




renderDiagram name a = renderSVG name dimensions a
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
	range = fromIntegral $ mapRange

	node :: Int -> Diagram B
	node n = circle 0.2 # fc black # lwG 0 # named n

selectRelations g comp friendss start = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss

createMaps g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral $ mapRange
createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map ((V.filter (>=0)).(V.map (fromID.(+(-(id start))))))

peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * mapRange).position) p, fromID $ (id p) - (id $ V.head people))


genPopulationMap2 :: String -> String -> IO ()
genPopulationMap2 n1 n2 = mapM_ (\index -> do
		let (g,_,_) = createGeneration index
		let name = "data/populationMap" ++ (show index) ++ ".svg"
		let range = fromIntegral $ mapRange

		renderDiagram name $ populationMapToDiagram $ VB.map V.length $ peopleMap (range*range) g
		) [read n1..read n2]

populationMapToDiagram :: VB.Vector Int -> Diagram B
populationMapToDiagram population = (populationSquares # center) ||| example
	where
	populationSquares :: Diagram B
	populationSquares = gridCat $ map (sq . fromIntegral) $ VB.toList population

	example :: Diagram B
	example = vsep biggestPopulation $ [sq biggestPopulation ||| f (show $ floor biggestPopulation), sq (biggestPopulation / 2), hsep biggestPopulation [sq 1, f "1"]]
		where f s = scale biggestPopulation $ text s <> rect (fromIntegral $ length s) 1 # lw n

	sq s = square s # fc black # lwG 0
--	sq s = roundedRect s s (s / 4) # fc black # lwG 0

	n = Diagrams.Prelude.none

	biggestPopulation = fromIntegral $ F.maximum population :: Double


genPopulationMap n1 n2 = let range = [read n1..read n2] in mapM_ (\(index, name) -> do
	let (g,_,_) = createGeneration index
	let g' = V.filter alive g
	let positions = let list = V.filter (/=(0,0)) $ V.map position g' in [[((x,y), V.length $ V.filter (\(x',y') -> x == x' && y == y') list) | x <- [0..mapRange]] | y <- [0..mapRange]]
	let f x--let r = x * 4 in if r > 255 then 255 else r
		| x == 0 = 0
		| x < 10 = 50
		| x < 50 = 50 + x * 3
		| otherwise = 200
	let toText :: ((Int32, Int32), Int) -> String; toText ((x, y), a) = (show x) ++ " " ++ (show y) ++ " " ++ (show $ 255 - f a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n"
	t <- return $ concat $ map toText $ concat $ positions
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
	let (g,_,_) = createGeneration index
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
	let (g,_,_) = createGeneration index
	let !g' = V.filter alive g
	let !positions = [[((x,y), f p) | x <- [0..mapRange], let p = V.filter ((\(x',y') -> x == x' && y == y').position) g'] | y <- [0..mapRange]]
	t <- return $ concat $ map (foldr (\((x,y),a) list -> (show x) ++ " " ++ (show y) ++ " " ++ (show a) ++ " " ++ "0" ++ " " ++ "0" ++ " " ++ (show $ if a == 0 then 0 else 255) ++ "\n" ++ list) "") positions
--	forkIO $ do
	h <- openBinaryFile name WriteMode
	hPutStrLn h t
	putStrLn $ "Written: " ++ name
	hClose h)
	$ zip range $ map (\i -> "data/professionMap" ++ (show i) ++ ".txt") range


calculateDistanceGraph :: VB.Vector (Vector Int) -> VB.Vector (Vector Int)
calculateDistanceGraph graph = VB.generate (VB.length graph) (\i -> V.generate (V.length (graph .! i)) (\l -> pathLength 0 l i))
	where
	pathLength :: Int -> Int -> Int -> Int
	pathLength depth goal current
		| depth == 3 || found = depth
		| otherwise = V.minimum $ V.map (pathLength (depth+1) goal) (graph .! current)
		where found = V.elem goal (graph .! current)

createGeneration n = generations seed 0 n (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	where p = start peopleFromStart

generations :: StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations seed (i+1) n $ ((change gen).(birth gen).(death gen)) previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i

