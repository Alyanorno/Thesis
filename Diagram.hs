{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Diagram (renderDiagram, mapRelations, populationMapToDiagram) where

import Prelude hiding (id)

import Diagrams.Prelude hiding (start, position, index)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Grid

import qualified Data.Vector as VB

import Control.Monad.ST()

import Stuff


renderDiagram name a = renderSVG name dimensions a
	where dimensions = mkSizeSpec2D (Just 1000) (Just 1000)

mapRelations :: [[Int]] -> VB.Vector [Int] -> VB.Vector [Int] -> Diagram B
mapRelations p r sr = baseLines <> redLines <> circles
	where
	baseLines :: Diagram B
	baseLines = drawAllNodes node p # applyAll (createArrows p r standardArrow) # opacityGroup 0.4

	redLines :: Diagram B
	redLines = drawAllNodes node p # applyAll (createArrows p sr redArrow) # opacityGroup 0.7

	circles :: Diagram B
	circles = drawAllNodes node p

	drawAllNodes nodeType p = foldr1 (<>) $ zipWith nodesInCircle p list
		where
		list = [r2 (x,y) | x <- [0, spacing..(range-1)*spacing], y <- [0, spacing..(range-1)*spacing]]

		nodesInCircle p offset
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

biggestPopulation = 100 :: Double

populationMapToDiagram :: VB.Vector Int -> Diagram B
populationMapToDiagram population = hsep biggestPopulation [((populationSquares # center) `atop` square (biggestPopulation * (fromIntegral mapRange))), example]
	where
	populationSquares :: Diagram B
	populationSquares = gridCat $ map ({-(((sq biggestPopulation :: D V2 Double) # phantom) `atop`) .-} sq . (\x -> if x > biggestPopulation then biggestPopulation else x) . fromIntegral) $ VB.toList population

	example :: Diagram B
	example = vsep biggestPopulation $ [sq biggestPopulation ||| f (show $ (floor biggestPopulation :: Int)), sq (biggestPopulation * (3/4)), sq (biggestPopulation / 2), sq (biggestPopulation / 4), hsep biggestPopulation [sq 1, f "1"]]
		where f s = scale biggestPopulation $ text s <> rect 3 1 # lw n

	sq s = square s # fc red # lwG 0
--	sq s = roundedRect s s (s / 4) # fc black # lwG 0

	n = Diagrams.Prelude.none

--	biggestPopulation = fromIntegral $ F.maximum population :: Double

