{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Stuff where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Unboxed ((!), toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Data.Int (Int64)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad.ST
import Control.Exception.Base (assert)


--{-# INLINE (<$>) #-}
--(<$>) = V.map
--{-# INLINE (<*>) #-}
--(<*>) = V.zipWith ($)

{-# INLINE (.&&.) #-}
(.&&.) f g !a = (f a) && (g a)
{-# INLINE (.||.) #-}
(.||.) f g !a = (f a) || (g a)

--data Culture = Culture { conservative :: Float, groupDivision :: Float }
--type Cultures = [Culture]
data Culture = Brahmatic | Endorphi deriving (Show, Eq, Enum, Bounded)
type Cultures = [Culture]

--data Goods = Goods { name :: String, needs :: [Goods], timePerUnit :: Int }
--data Proffesion = Proffesion { makes :: [Goods] }
data Profession = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
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
type People = VB.Vector Person
-- http://stackoverflow.com/questions/10866676/how-do-i-write-a-data-vector-unboxed-instance-in-haskell

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

{-# INLINE (&!) #-}
(&!) :: People -> ID -> Person
(&!) people i = people .! (i - (id $ VB.head people))

safeAccess :: People -> Int -> (Bool, Person)
safeAccess people i = let ix = i - (id $ VB.head people) in if i < id (VB.head people) then (False, VB.head people) else (True, people .! ix)

{-# INLINE (.!) #-}
(.!) :: VB.Vector a -> Int -> a
(.!) v i = v VB.! i

--vsplitPlaces :: (Eq e) => Vector Int -> Vector e -> Vector (Vector e)
--vsplitPlaces places list
--	| V.length places == 0 = (fromList [])
--	| otherwise = snoc (vsplitPlaces (V.tail places) (V.drop (V.head places) list)) (V.take (V.head places) list)

--randomVectorsOf :: Int -> Int -> RandomGenerator -> (Float, Float) -> VB.Vector (A.UArray Int Float)
--randomVectorsOf n size gen range = VB.generate size (\i -> A.listArray (0, n) (randomRs range (Xorshift $ xor (fromXorshift gen) $ fromIntegral i)))
--randomVectorsOf_ :: Int -> Int -> RandomGenerator -> (Int, Int) -> VB.Vector (A.UArray Int Int)
--randomVectorsOf_ n size gen range = VB.generate size (\i -> A.listArray (0, n) (randomRs range (Xorshift $ xor (fromXorshift gen) $ fromIntegral i)))
randomVector :: Int -> PureMT -> Vector Int64
randomVector n g = if n <= 0 then V.empty else V.create $ do { v <- M.new n; fill v 0 g; return v }
	where
		fill v i g
			| i < n = do
				(x, g') <- return $ R.randomInt64 g
				M.write v i x
				fill v (i+1) g'
			| otherwise = return ()
randomVector_ :: Int -> PureMT -> Vector Double
randomVector_ n g = if n <= 0 then V.empty else V.create $ do { v <- M.new n; fill v 0 g; return v }
	where
		fill v i g
			| i < n = do
				(x, g') <- return $ R.randomDouble g
				M.write v i x
				fill v (i+1) g'
			| otherwise = return ()


rescale :: Int -> Int -> Int -> Int
rescale maxX maxY a = floor $ (fromIntegral a) * ((fromIntegral maxY) / (fromIntegral maxX))

rescale_ :: Int -> Float -> Int -> Float
rescale_ maxX maxY a = (fromIntegral a) * (maxY / (fromIntegral maxX))

-- TODO: More even spread of age
start :: Int -> People
start a = let off = a * 3 in VB.fromList $ (take off $ repeat (Person False 1 70 Male Farmer Endorphi 0 (0,0) V.empty [] (0,0))) ++ [Person True i age g prof cult (if i >= a then 0 else 1) (if i >= a then (1+i-a,1+i-a) else (0,0)) V.empty [] (mapRange `div` 2, mapRange `div` 2) | (i,(g,(prof,cult))) <- zip [off+1..off+1+a*2] $ zip (cycle [Male, Female]) $ zip (infinitly [minBound::Profession .. maxBound::Profession]) (infinitly [minBound::Culture .. maxBound::Culture]), let age = f i a]
	where
		f i a
			| i >= float2Int ((int2Float) a * 1.5) = 0
			| i >= a = 20
			| i >= a `div` 2 = 40
			| otherwise = 60
		infinitly x = cycle $ concat $ zipWith (\a b -> a : b : []) x x

infinity :: Float
infinity = 1 / 0

mapRange :: Int
mapRange = 50

timeStep :: Int
timeStep = 2 -- If 1 they will never reproduce :P (birth rounds down)

peopleFromStart :: Int
peopleFromStart = 50

scaleDistanceFromCenter :: Float -> Float
scaleDistanceFromCenter = (*) (0-0.1) --0.001

scaleDistanceFromCulturalCenter :: Float -> Float
scaleDistanceFromCulturalCenter = (*) (0-0.1)

scaleConcentrationOfPeople :: Float -> Float
scaleConcentrationOfPeople x = if x < -50000 then infinity else (0-x) ** 1.2 -- ( (-) 0).((**) 1.5)

scaleCulturalMap :: Float -> Float
scaleCulturalMap = (*1)

staticTerrainMap :: Vector Float
staticTerrainMap = V.fromList $ map (\x -> if x == 1 then -10000 else x) $ map (\x -> if x == 2 then infinity else x) $ concat --(V.fromList $ take (mapRange*mapRange) $ repeat 0.0) V.// mountain
	[
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]
	where
--		mountain = zip (map fromRelativePosition mountainPositions)
--		(repeat infinity) --(repeat (0-100)) mountainPositions = [(x,y)
--		| x <- [0.2,0.21..1], y <- [0.2,0.21..0.3]]
		fromRelativePosition :: (Float, Float) -> Int
		fromRelativePosition (x,y) = (float2Int (x * m)) + (float2Int (y * m * m))
			where m = int2Float mapRange


-- TODO: Add values for static relations
staticProfessionalRelations :: VB.Vector (Vector Float)
staticProfessionalRelations = let fl = V.fromList in VB.fromList [fl [0,0,0,0], fl [0,0,0,0], fl [0,0,0,0], fl [0,0,0,0]]

boxFilter :: Vector Float -> Vector Float
boxFilter list = V.imap (\i a -> let f = access a in (a + (f $ i-1) + (f $ i+1) + (f $ i-mapRange) + (f $ i+mapRange) / 5)) list
	where
		access a i = if i < 0 || i >= V.length list then a else list ! i

