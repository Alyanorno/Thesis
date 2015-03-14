{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stuff where

import Prelude hiding (id)

import Data.Vector ((!), fromList, toList, Vector)
import qualified Data.Vector as V

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import Control.Monad.ST


(<$>) = V.map
(<*>) = V.zipWith ($)

--data Culture = Culture { conservative :: Float, groupDivision :: Float }
--type Cultures = [Culture]
data Culture = Brahmatic | Endorphi deriving (Show, Eq, Enum, Bounded)
type Cultures = [Culture]

--data Goods = Goods { name :: String, needs :: [Goods], timePerUnit :: Int }
--data Proffesion = Proffesion { makes :: [Goods] }
data Profession = Administrator | Farmer | Beggar | None deriving (Show, Eq, Enum, Bounded)
type Professions = [Profession]

type ID = Int
data Gender = Male | Female deriving (Show, Eq)
-- data Proffesion = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
data Person = Person { id :: ID, age :: Int, gender :: Gender, profession :: Profession, culture :: Culture, lover :: ID, parrents :: (ID, ID), children :: Vector ID, friends :: [ID] } deriving (Show)
type People = Vector Person

instance Eq Person where
	p1 == p2 = id p1 == id p2

type World = ( People, Professions, Cultures )




newtype Xorshift = Xorshift Int64 deriving (Show, Eq, Enum, Bounded)

step :: Xorshift -> Xorshift
step (Xorshift a) = Xorshift d where
	b = xor a (shiftL a 13)
	c = xor b (shiftR b 7)
	d = xor c (shiftL c 17)

instance RandomGen Xorshift where
	next a = (fromIntegral c, b)
		where b@(Xorshift c) = step a

	split  = error "Splitting on Xorshift not implemented"

	genRange a = (fromEnum (asTypeOf minBound a), fromEnum (asTypeOf maxBound a))


type RandomGenerator = Xorshift
--type RandomGenerator = StdGen




randomListsOf n gen = chunksOf 5 (randomRs (0, 1) gen :: [Float])
randomListsOf_ n gen = chunksOf 5 (randomRs (0, 1) gen :: [Float])

rescale :: Int -> Int -> Int -> Int
rescale maxX maxY a = floor $ (fromIntegral a) * ((fromIntegral maxY) / (fromIntegral maxX))


start :: Int -> People
start a = fromList [Person i 20 g None Endorphi 0 (0,0) V.empty [] | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 

