{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (start, position)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Grid

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
	putStrLn "Enter number of iterations: "
	n <- getLine
	let p = start peopleFromStart
	let (g, friendss,_) = generations seed 0 (read n) (p, VB.replicate (V.length p) V.empty, VB.replicate (V.length p) V.empty)
	let g' = V.filter alive g
	let g'' = V.filter (not.alive) g
	let toProcent x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g')
	let toProcent2 x = floor $ (*) 100 $ (int2Float x) / (int2Float $ V.length g)
	putStrLn ""
	putStrLn $ "Total: " ++ (show $ V.length g)
	putStrLn $ "Alive: " ++ (show $ toProcent2 $ V.length g') ++ "% "
	putStrLn $ "Lover: " ++ (show $ toProcent $ V.length $ V.filter (((/=0).lover) .&&. ((<41).age)) g') ++ "%"

	let r :: Int; r = fromIntegral $ mapRange
	let map' = VB.toList $ VB.map V.toList $ peopleMap (r*r) g
	let mapLengths' = VB.toList $ VB.map V.length $ peopleMap (r*r) g

	putStrLn $ show mapLengths'

	let fromIDtoGraph = VB.map ((V.filter (>=0)).(V.map (fromID.(+(-(id $ V.head g))))))
	let relations = VB.map V.toList $ fromIDtoGraph friendss
--	let relations = let t g = randomRs (0, length (concat map')) $ mkStdGen g in take 2000 $ zip (t 0) (t 123423)

	mainWith (test map' relations)

test :: [[Int]] -> VB.Vector [Int] -> Diagram B
test p r = (foldr1 (<>) $ zipWith nodesInCircle p [r2 (x,y) | x <- [0, spacing..(range-1)*spacing], y <- [0, spacing..(range-1)*spacing]]) # applyAll [connectOutside' arrowOptions from to | temp <- p, from <- temp, to <- r .! from] # opacityGroup 0.5 <> (foldr1 (<>) $ zipWith nodesInCircle p [r2 (x,y) | x <- [0, spacing..(range-1)*spacing], y <- [0, spacing..(range-1)*spacing]])
	where
	spacing = 50
	range = fromIntegral $ mapRange

arrowOptions = with & gaps .~ superTiny & arrowHead .~ tri' & headLength .~ local 0.15 & headStyle %~ fc black . opacity 0.02 & shaftStyle %~ fc black . opacity 0.01 . lw ultraThin
	where
	superTiny = normalized 0.002

nodesInCircle p offset
	| length p < 3 = strutX 1 # translate offset
	| otherwise = atPoints (trailVertices $ regPoly (length p) 1 # translate offset) (map node p)

node :: Int -> Diagram B
node n = circle 0.2 # fc green # lwG 0 # named n


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

