{-# LANGUAGE PartialTypeSignatures, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-ignore-asserts #-}

module Renderings (
	renderDiagram,
	renderConnections,
	renderHeatmap,
	renderPopulationMap,
	renderCultureMap,
	renderProfessionMap,
	renderMegaMap)
where

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

import Debug.Trace as T

import Stuff
import Definitions



biggestPopulation = 50 :: Double

renderMegaMap :: Options -> People -> Diagram B
renderMegaMap opt people = gridCat . map sq $ zip3 popMap cultMap profMap
	where
	popMap = populationMap (fromIntegral $ mapRange opt) people
	cultMap = map f $ cultureMap opt people
		where f c
			| sum c == 0 = 0
			| b == 0 = 1
			| b == 1 = 2
			| b == 2 = 3
			| otherwise = 0
			where b = snd $ L.maximumBy (comparing fst) $ zip c [0 :: Int ..]

	profMap = professionMap opt people

	sq :: (Int, Int, Float) -> Diagram B
	sq (s, cult, prof) = shape (max (fromIntegral s) 0.0001) # fc (blend mix red black) # lwG 0
		where
		shape :: Double -> Diagram B
		shape s = case cult of
			0 -> square s
			1 -> triangle s
			2 -> square s
			3 -> circle (s/2)

		mix = (float2Double $ prof / (L.maximum $ map (professionValue opt) allProfessions))


populationMap range = VB.toList . VB.map V.length . peopleMap (range*range)
renderPopulationMap opt people = hsep bp [(populationSquares # center) `atop` square (bp * fromIntegral range), example]
	where
	range = mapRange opt

	populationSquares = gridCat . map f $ populationMap (fromIntegral range) people
		where
		f = ((sq bp # phantom) `atop`) . sq . min bp . fromIntegral

	example = vsep bp [sq bp ||| f (show $ floor bp), sq (bp * (3/4)), sq (bp / 2), sq (bp / 4), hsep bp [sq 1, f "1"]]
		where f s = scale bp $ text s <> rect 3 1 # lw n

	sq :: Double -> Diagram B
	sq s = square s # fc red # lwG 0
	n = Diagrams.Prelude.none

	bp = biggestPopulation


cultureMap opt people = concat [[[f p c | c <- allCultures] | x <- [0..range-1], let p = V.filter ((\(x',y') -> x == x' && y == y').position) $ V.filter alive people] | y <- [0..range-1]]
	where
	range = fromIntegral $ mapRange opt
	f p c
		| V.null p = 0
		| otherwise = float2Int $ int2Float (V.length $ V.filter ((==c).culture) p) / int2Float (V.length p) * 255
renderCultureMap opt people = gridCat . map (sq . setColour) $ cultureMap opt people
	where
	setColour :: (Ord a, Floating a) => [Int] -> (Double, Colour a)
	setColour c
		| sum c == 0 = (0, white) --"0 0 0 0"
		| b == 0 = (1, red) --"255 0 0 255"
		| b == 1 = (1, green) --"0 255 0 255"
		| b == 2 = (1, blue) --"0 0 255 255"
		| otherwise = (0, white) --"0 0 0 0"
		where b = snd $ L.maximumBy (comparing fst) $ zip c [0 :: Int ..]

	sq (s, c) = square s # fc c # lwG 0


professionMap opt people = concat [[f p | x <- [0..range-1], let p = V.filter ((\(x',y') -> x == x' && y == y').position) $ V.filter alive people] | y <- [0..range-1]]
	where
	range = fromIntegral $ mapRange opt
	f :: People -> Float; f l
		| V.null l = 0
		| otherwise = let t = V.foldr1 (+) (V.map (professionValue opt . profession) l) / fromIntegral (V.length l); m = 1.5 in (if t > 2.5 then 2.5 else (if t < m then m else t)) - m
renderProfessionMap opt people = gridCat . map sq $ professionMap opt people
	where sq s = square s # fc red # lwG 0



renderConnections t name opt people friendss = renderDiagram name' (mapRelations range map r1 r2)
	where
	name' = "data/connections" ++ (toUpper (head name) : tail name) ++ ".svg"
	r1 = selectRelations' (not.comp)
	r2 = selectRelations' comp
	selectRelations' = selectRelations people friendss

	(map,_) = createMaps range people
	range = mapRange opt

	comp = case t of
		"cult" -> (== stringToCulture name).culture
		"prof" -> (== stringToProfession name).profession
		_ -> error "Second argument for connections must be either \"cult\" or \"prof\""

renderHeatmap t name opt (people,_) = renderDiagram name' $ renderPopulationMap opt $ V.filter (alive .&&. comp) people
	where
	name' = "data/map" ++ (toUpper (head name) : tail name) ++ ".svg"

	comp = case t of
		"cult" -> (== stringToCulture name).culture
		"prof" -> (== stringToProfession name).profession
		_ -> error "Second argument for map must be either \"cult\" or \"prof\""


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
	range = fromIntegral mapRange

	node :: Int -> Diagram B
	node n = circle 0.2 # fc black # lwG 0 # named n

peopleMap :: Int -> People -> VB.Vector (Vector Int)
peopleMap size people = VB.unsafeAccumulate V.snoc (VB.replicate size V.empty) $ VB.map f $ VB.filter ((/=(0,0)).position) $ VB.convert people
	where f p = ((fromIntegral.(\(x,y) -> x + y * round (sqrt $ fromIntegral size)).position) p, fromID $ id p - id (V.head people))

selectRelations g friendss comp = VB.imap (\i a -> if (comp.(g !)) i then a else filter (comp.(g !)) a) $ createRelations start friendss
	where start = V.head g
createRelations start f = VB.map V.toList $ fromIDtoGraph f
	where fromIDtoGraph = VB.map (V.filter (>=0) . V.map (fromID.(+(-(id start)))))
createMaps mapRange g = (VB.toList $ VB.map V.toList $ peopleMap (r*r) g, VB.toList $ VB.map V.length $ peopleMap (r*r) g)
	where r :: Int; r = fromIntegral mapRange



