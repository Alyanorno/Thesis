{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude hiding (start, position)
import Diagrams.Backend.SVG
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Size

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import qualified Data.List as L
import Data.List.Split (chunksOf, splitPlaces)

import Data.Char (toUpper)
import Data.Int (Int32, Int64)
import Data.Ord
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
	n <- return $ args !! 0
	let p = start peopleFromStart
	let (g, friendss,_) = generations seed 0 (read n) (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	let g' = V.filter alive g
	let g'' = V.filter (not.alive) g
	let toProcent x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g')
	let toProcent2 x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g)
	putStrLn ""
--	putStrLn $ "Total: " ++ (show $ V.length g)
--	putStrLn $ "Alive: " ++ (show $ toProcent2 $ V.length g') ++ "% "
--	putStrLn $ "Lover: " ++ (show $ toProcent $ V.length $ V.filter (((/=0).lover) .&&. ((<41).age)) g') ++ "%"

	let (map', mapLengths') = createMaps g -- $ V.filter ((/=farmer).profession) g
--	putStrLn $ show mapLengths'

--	let relations = createRelations (V.head g) friendss
--	let relations = VB.map (filter ((/=farmer).profession.(g !))) $ createRelations (V.head g) friendss
--	let relations = VB.imap (\i a -> if ((/=farmer).profession.(g !)) i then [] else a) $ createRelations (V.head g) friendss
--	let relations = let t g = randomRs (0, length (concat map')) $ mkStdGen g in take 2000 $ zip (t 0) (t 123423)

--	mainWith (mapRelations map' relations)

	let comp = case args !! 1 of
		"cult" -> (==(stringToCulture $ args !! 2)).culture
		"prof" -> (==(stringToProfession $ args !! 2)).profession
		_ -> error "Second argument must be either \"cult\" or \"prof\""

	let name = "diagram" ++ (let a = args !! 2 in (toUpper $ head a) : (tail a)) ++ ".png"
--	let r1 = createRelations (V.head g) friendss
	let r1 = selectRelations g (not.comp) friendss (V.head g)
	let r2 = selectRelations g comp friendss (V.head g)

	putStrLn "Started"
	renderDiagram name map' r1 r2
--	R.mainWith (mapRelations map' r1 r2)
	putStrLn "Done"


selectRelations g comp friendss start = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss


--renderDiagram name map' r1 r2 = renderSVG name dimensions (mapRelations map' r1 r2)
renderDiagram name map' r1 r2 = renderSVG name dimensions (mapRelations map' r1 r2)
	where dimensions = mkSizeSpec2D (Just 1000) (Just 1000)


createMaps g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral $ mapRange
createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map ((V.filter (>=0)).(V.map (fromID.(+(-(id start))))))


mapRelations :: [[Int]] -> VB.Vector [Int] -> VB.Vector [Int] -> Diagram B
mapRelations p r sr = baseLines <> redLines <> circles
	where
	baseLines = drawAllNodes node p # applyAll (createArrows p r standardArrow) # opacityGroup 0.4
	redLines = drawAllNodes node p # applyAll (createArrows p sr redArrow) # opacityGroup 0.7
	circles = drawAllNodes node p



	drawAllNodes nodeType p = foldr1 (<>) $ zipWith (nodesInCircle nodeType) p list
		where list = [r2 (x,y) | x <- [0, spacing..(range-1)*spacing], y <- [0, spacing..(range-1)*spacing]]

	createArrows p r o = [connectOutside' o from to | temp <- p, from <- temp, to <- r .! from]

	standardArrow = with
		& gaps .~ superTiny
		& arrowHead .~ tri'
		& headLength .~ local 0.15
		& headStyle %~ lc black . opacity 0.02
		& shaftStyle %~ lc black . opacity 0.01 . lw ultraThin
	redArrow = with
		& gaps .~ superTiny
		& arrowHead .~ tri'
		& headLength .~ local 0.15
		& headStyle %~ lc red . opacity 0.02
		& shaftStyle %~ lc red . opacity 0.01 . lw ultraThin

	superTiny = normalized 0.002

	spacing = 15
	range = fromIntegral $ mapRange

emptyNode :: Int -> Diagram B
emptyNode n = strut (r2 (0.2, 0.2)) # named n

node :: Int -> Diagram B
node n = circle 0.2 # fc black # lwG 0 # named n


nodesInCircle nodeType p offset
	| length p < 3 = strutX 1 # translate offset
	| otherwise = atPoints (trailVertices $ regPoly (length p) 1 # translate offset) (map nodeType p)



peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * mapRange).position) p, fromID $ (id p) - (id $ V.head people))


generations :: StdGen -> Int -> Int -> (People, Friends, Childrens) -> (People, Friends, Childrens)
generations seed i n previous
	| i >= n = previous
	| otherwise = next
	where 
		next = generations seed (i+1) n $ ((change gen).(birth gen).(death gen)) previous
		gen = Xorshift $ (randomRs (minBound :: Int64, maxBound :: Int64) seed :: [Int64]) !! i

