{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stuff where

import Prelude hiding (id)

import Data.Vector ((!), fromList, toList, Vector, snoc)
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

(.&&.) f g a = (f a) && (g a)
(.||.) f g a = (f a) || (g a)

--data Culture = Culture { conservative :: Float, groupDivision :: Float }
--type Cultures = [Culture]
data Culture = Brahmatic | Endorphi deriving (Show, Eq, Enum, Bounded)
type Cultures = [Culture]

--data Goods = Goods { name :: String, needs :: [Goods], timePerUnit :: Int }
--data Proffesion = Proffesion { makes :: [Goods] }
data Profession = Administrator | Farmer | Beggar | None deriving (Show, Eq, Enum, Bounded)
type Professions = [Profession]

type ID = Int
data Gender = Male | Female deriving (Show, Eq, Enum)
-- data Proffesion = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
data Person = Person {
	alive :: Bool,
	-- Unique number used to identifiy person, people are always stored in order
	id :: ID,
	age :: Int,
	gender :: Gender,
	profession :: Profession,
	culture :: Culture,
	-- A zero means no lover
	lover :: ID,
	parrents :: (ID, ID),
	children :: Vector ID,
	friends :: [ID],
	-- Below a certain age this attribute is ignored
	position :: (Int, Int)
	} deriving (Show)
type People = Vector Person

instance Eq Person where
	p1 == p2 = id p1 == id p2

type World = ( People, Professions, Cultures )




newtype Xorshift = Xorshift {fromXorshift :: Int64} deriving (Show, Eq, Enum, Bounded)

step :: Xorshift -> Xorshift
step (Xorshift a) = Xorshift d where
	b = xor a (shiftL a 13)
	c = xor b (shiftR b 7)
	d = xor c (shiftL c 17)

instance RandomGen Xorshift where
	next a = (fromIntegral c, b)
		where b@(Xorshift c) = step a

	split = error "Splitting on Xorshift not implemented"

	genRange a = (fromEnum (asTypeOf minBound a), fromEnum (asTypeOf maxBound a))


type RandomGenerator = Xorshift
--type RandomGenerator = StdGen

(.!) :: People -> ID -> Person
(.!) people i = people ! (i - (id $ people ! 0))

--vsplitPlaces :: (Eq e) => Vector Int -> Vector e -> Vector (Vector e)
--vsplitPlaces places list
--	| V.length places == 0 = (fromList [])
--	| otherwise = snoc (vsplitPlaces (V.tail places) (V.drop (V.head places) list)) (V.take (V.head places) list)

--randomVectorsOf n size gen range = generate size (\i -> generate n $ xor gen $ step $ fromInteger $ i * n)

randomVectorsOf :: Int -> Int -> RandomGenerator -> (Float, Float) -> Vector [Float]
randomVectorsOf n size gen range = V.generate size (\i -> take n (randomRs range (Xorshift $ xor (fromXorshift gen) $ fromIntegral i)) :: [Float])
randomVectorsOf_ :: Int -> Int -> RandomGenerator -> (Int, Int) -> Vector [Int]
randomVectorsOf_ n size gen range = V.generate size (\i -> take n (randomRs range (Xorshift $ xor (fromXorshift gen) $ fromIntegral i)) :: [Int])

rescale :: Int -> Int -> Int -> Int
rescale maxX maxY a = floor $ (fromIntegral a) * ((fromIntegral maxY) / (fromIntegral maxX))

rescale_ :: Int -> Float -> Int -> Float
rescale_ maxX maxY a = (fromIntegral a) * (maxY / (fromIntegral maxX))

start :: Int -> People
start a = fromList [Person True i 20 g None Endorphi 0 (0,0) V.empty [] (0,0) | (i,g) <- zip [0..a] (cycle [Male, Female]) ] 

