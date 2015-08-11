{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Stuff where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (toList, Vector, snoc)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Array.Unboxed as A

import Data.List.Split (chunksOf)
import qualified Data.List as L

import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.Storable.Newtype as Store
import Data.Int
import Data.Word
import Data.Bits
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Function (fix)
import Data.Bits (xor, shiftL, shiftR)
import System.Random
import System.Random.Mersenne.Pure64 as R
import Control.Monad (liftM)
import Control.Monad.ST
import Control.Exception.Base (assert)


{-# INLINE (.&&.) #-}
(.&&.) f g !a = (f a) && (g a)
{-# INLINE (.||.) #-}
(.||.) f g !a = (f a) || (g a)

newtype Culture = Culture {fromCulture :: Word8} deriving (Show, Eq)
(brahmatic:endorphi:uppbrah:_) = map Culture [0..]
allCultures = [brahmatic, endorphi, uppbrah]
allCulturesVector = VB.fromList ([0,1,2] :: [Int])
cultureToInt :: Culture -> Int
cultureToInt (Culture c) = fromIntegral c

--data Profession = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
--type Professions = [Profession]
newtype Profession = Profession {fromProfession :: Word8} deriving (Show, Eq)
(farmer:administrator:beggar:none:_) = map Profession [0..]
allProfessions = [farmer, administrator, beggar, none]
allProfessionsVector = VB.fromList ([0,1,2,3] :: [Int])
professionToInt :: Profession -> Int
professionToInt (Profession p) = fromIntegral p

newtype ID = ID Int32 deriving (Show, Eq, Ord, Num)
{-# INLINE fromID #-}
fromID (ID id) = fromIntegral id
{-# INLINE toID #-}
toID id = ID $ fromIntegral id

instance Storable ID where
	sizeOf _ = sizeOf (undefined :: Int32)
	alignment _ = sizeOf (undefined :: Int32)
	{-# INLINE peek #-}
	peek p = liftM ID (peekElemOff (castPtr p :: Ptr Int32) 0)
	{-# INLINE poke #-}
	poke p (ID id) = pokeElemOff (castPtr p :: Ptr Int32) 0 id

--data Gender = Male | Female deriving (Show, Eq, Enum)
newtype Gender = Gender {fromGender :: Word8} deriving (Show, Eq)
(male:female:_) = map Gender ([0..] :: [Word8])

alive :: Person -> Bool
alive p = if dead p == 0 then True else False
data Person = Person {
	dead :: {-# UNPACK #-} !Word8,
	gender :: {-# UNPACK #-} !Gender,
	profession :: {-# UNPACK #-} !Profession,
	culture :: {-# UNPACK #-} !Culture,
	age :: {-# UNPACK #-} !Int32,
	-- Unique number used to identifiy person, people are always stored in order
	id :: {-# UNPACK #-} !ID,
	-- A zero means no lover
	lover :: {-# UNPACK #-} !ID,
	parrents :: {-# UNPACK #-} !(ID, ID),
	-- Below a certain age this attribute is ignored
	position :: {-# UNPACK #-} !(Int32, Int32)
	} deriving (Show)
type People = Vector Person

instance Storable Person where
	sizeOf _ = sizeOf (undefined :: Int32) * 8
	alignment _ = alignment (undefined :: Int32)

	{-# INLINE peek #-}
	peek p = do
		let q2 = castPtr p :: Ptr Word8
		dead <- peekElemOff q2 0
		gender <- peekElemOff q2 1
		profession <- peekElemOff q2 2
		culture <- peekElemOff q2 3

		let q = castPtr p :: Ptr Int32
		age <- peekElemOff q 1
		id <- peekElemOff q 2
		lover <- peekElemOff q 3
		parrents1 <- peekElemOff q 4
		parrents2 <- peekElemOff q 5
		position1 <- peekElemOff q 6
		position2 <- peekElemOff q 7
		return (Person dead (Gender gender) (Profession profession) (Culture culture) (fromIntegral age) (fromIntegral id) (fromIntegral lover) (fromIntegral parrents1, fromIntegral parrents2) (fromIntegral position1, fromIntegral position2))

	{-# INLINE poke #-}
	poke p (Person dead gender profession culture age id lover (parrents1, parrents2) (position1, position2)) = do
		let q2 = castPtr p :: Ptr Word8
		pokeElemOff q2 0 dead
		pokeElemOff q2 1 $ fromGender gender
		pokeElemOff q2 2 $ fromProfession profession
		pokeElemOff q2 3 $ fromCulture culture

		let q = castPtr p :: Ptr Int32
		pokeElemOff q 1 (age :: Int32)
		pokeElemOff q 2 (fromID id :: Int32)
		pokeElemOff q 3 (fromID lover :: Int32)
		pokeElemOff q 4 (fromID parrents1 :: Int32)
		pokeElemOff q 5 (fromID parrents2 :: Int32)
		pokeElemOff q 6 (position1 :: Int32)
		pokeElemOff q 7 (position2 :: Int32)


instance Eq Person where
	p1 == p2 = id p1 == id p2

type Childrens = VB.Vector (Vector ID)
type Friends = VB.Vector (Vector ID)


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



{-# INLINE (!) #-}
(!) v i = V.unsafeIndex v $ fromIntegral i

{-# INLINE (.!) #-}
(.!) :: (Integral i) => VB.Vector a -> i -> a
(.!) v i = VB.unsafeIndex v $ fromIntegral i

{-# INLINE (&!) #-}
(&!) :: People -> ID -> Person
(&!) people i = V.unsafeIndex people $ fromID (i - (id $ V.head people))

safeAccess :: People -> ID -> (Bool, Person)
safeAccess people i = let ix = i - (id $ V.head people) in if i < id (V.head people) then (False, V.head people) else (True, people ! (fromID ix))


randomVector :: Int -> PureMT -> Vector Int64
randomVector n g = if n <= 0 then V.empty else V.create $ do { v <- M.new n; fill v 0 g; return v }
	where
		fill v i g
			| i < n = do
				(x, g') <- return $ R.randomInt64 g
				M.write v i x
				fill v (i+1) g'
			| otherwise = return ()
randomVector_ :: Int -> PureMT -> (Vector Double, PureMT)
randomVector_ n g = if n <= 0 then (V.empty, g) else runST $ do { v <- M.new n; g' <- fill v 0 g; v' <- V.unsafeFreeze v; return (v', g')}
	where
		fill v i g
			| i < n = do
				(x, g') <- return $ R.randomDouble g
				M.write v i x
				fill v (i+1) g'
			| otherwise = return g


--rescale :: Int -> Int -> Int -> Int
rescale maxX maxY a = floor $ (fromIntegral a) * ((fromIntegral maxY :: Float) / (fromIntegral maxX :: Float))

--rescale_ :: Int -> Float -> Int -> Float
rescale_ maxX maxY a = (fromIntegral a) * (maxY / (fromIntegral maxX))


start :: Int -> People
start a = let off = a * 3 in V.fromList $ (take (fromIntegral off) $ repeat (Person 1 male farmer endorphi 70 1 0 (0,0) (0,0))) ++ [Person 0 g prof cult age (toID i) (if i >= a then 0 else 1) (if i >= a then (toID (1+i-a), toID (1+i-a)) else (ID 0,ID 0)) (mapRange `div` 2, mapRange `div` 2) | (i,(g,(prof,cult))) <- zip [off+1..off+1+a*2] $ zip (cycle [male, female]) $ zip (infinitly allProfessions) (infinitly allCultures), let age = fromIntegral $ f (i-off) a]
	where
		f :: Int -> Int -> Int
		f i max
			| i >= floor ((fromIntegral max :: Float) * 0.75) = 0
			| i >= floor ((fromIntegral max :: Float) * 0.30) = 20
			| i >= floor ((fromIntegral max :: Float) * 0.10) = 40
			| otherwise = 60
		infinitly x = cycle $ concat $ zipWith (\a b -> a : b : []) x x

distanceTo :: (Float,Float) -> (Float,Float) -> Float
distanceTo (x,y) (x',y') = (x-x')^2 + (y-y')^2

toFloat :: (Integral i) => (i,i) -> (Float,Float)
toFloat (x,y) = (fromIntegral x, fromIntegral y)

infinity :: Float
infinity = 1 / 0

mapRange :: Int32
mapRange = 50

timeStep :: Int
timeStep = 4 -- If 1 they will never reproduce :P (birth rounds down)

peopleFromStart :: Int
peopleFromStart = 50

scaleDistanceFromCenter :: Float -> Float
--scaleDistanceFromCenter = (*) (0-0.1) --0.001
scaleDistanceFromCenter a = 100 - a * 1

scaleDistanceFromCulturalCenter :: Float -> Float
scaleDistanceFromCulturalCenter = (*) (0-0.1)

scaleConcentrationOfPeople :: Float -> Float
scaleConcentrationOfPeople x = if x < -50000 then infinity else (0-x) ** 1.2 -- ( (-) 0).((**) 1.5)

scaleCulturalMap :: Float -> Float
scaleCulturalMap = (*1)

staticTerrainMap :: Vector Float
staticTerrainMap = V.fromList $ [base ! ((rescale mapRange 50 x) + (rescale mapRange 50 y) * 50) | x <- [0..mapRange-1], y <- [0..mapRange-1]]
	where base = V.fromList $ map (\x -> if x == 1 then -10000 else x) $ map (\x -> if x == 2 then infinity else x) $ concat --(V.fromList $ take (mapRange*mapRange) $ repeat 0.0) V.// mountain
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
			fromRelativePosition :: (Float, Float) -> Int
			fromRelativePosition (x,y) = (float2Int (x * m)) + (float2Int (y * m * m))
				where m :: Float; m = fromIntegral mapRange


staticProfessionalRelations :: VB.Vector (Vector Float)
staticProfessionalRelations = let fl = V.fromList in VB.fromList [fl [1,1,0,0], fl [1,1,0,0], fl [0,0,1,1], fl [0,0,1,1]]

professionValue prof
	| prof == farmer = 1
	| prof == administrator = 2
	| prof == beggar = 0
	| prof == none = 0
	| otherwise = 0

boxFilter :: Vector Float -> Vector Float
boxFilter list = V.imap (\i a -> let f = access a in (a + (f $ i-1) + (f $ i+1) + (f $ i-(fromIntegral mapRange)) + (f $ i+(fromIntegral mapRange)) / 5)) list
	where
		access a i = if i < 0 || i >= V.length list then a else list ! i

