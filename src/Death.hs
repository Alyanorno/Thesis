{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Death where

import Prelude hiding (id)
import GHC.Float

import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V

import System.Random.Mersenne.Pure64 as R

import Definitions
import Stuff


death :: Xorshift -> (People, Friends, Childrens) -> (People, Friends, Childrens)
death gen (people, friends, childrens) = (V.zipWith foo people' r, friends', childrens')
	where
		friends' = VB.take (V.length people') friends
		childrens' = VB.take (V.length people') childrens

		r = V.fromList $ map double2Float $ take (V.length people') $ f $ pureMT $ fromIntegral $ fromXorshift gen
			where f g = let (v,g') = R.randomDouble g in v : f g'

		people' = V.filter ((<80).age) $ V.map (\a -> a {age = (age a) + (fromIntegral timeStep)}) people
		foo :: Person -> Float -> Person
		foo p c
			| a < 20 = if c < 0.992414 .^ timeStep' then p else p {dead = 1} 
			| a < 30 = if c < 0.997491 .^ timeStep' then p else p {dead = 1}
			| a < 40 = if c < 0.997491 .^ timeStep' then p else p {dead = 1}
			| a < 50 = if c < 0.994962 .^ timeStep' then p else p {dead = 1}
			| a < 60 = if c < 0.992414 .^ timeStep' then p else p {dead = 1}
			| a < 70 = if c < 0.987259 .^ timeStep' then p else p {dead = 1}
			| a < 80 = if c < 0.974004 .^ timeStep' then p else p {dead = 1}
			| otherwise = p {dead = 1}
			where a = age p; (.^) a1 a2 = (**) a1 a2
		timeStep' :: Float
		timeStep' = fromIntegral timeStep
