{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Definitions where

import Prelude hiding (id)

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB

import Foreign.Ptr
import Foreign.Storable
import Data.Int
import Data.Word
import Data.Bits
import System.Random
import Control.Monad (liftM)


newtype Culture = Culture {fromCulture :: Word8} deriving (Show, Eq)
(brahmatic:endorphi:uppbrah:_) = map Culture [0..]
allCultures = [brahmatic, endorphi, uppbrah]
allCulturesVector = VB.fromList ([0,1,2] :: [Int])
cultureToInt :: Culture -> Int
cultureToInt (Culture c) = fromIntegral c
stringToCulture :: String -> Culture
stringToCulture a = case a of
	"brahmatic" -> brahmatic
	"endorphi" -> endorphi
	"uppbrah" -> uppbrah
	_ -> error "Incorrect culture name"

--data Profession = Farmer | Administrator | Beggar | None deriving (Show, Eq, Enum, Bounded)
--type Professions = [Profession]
newtype Profession = Profession {fromProfession :: Word8} deriving (Show, Eq)
(farmer:administrator:beggar:none:_) = map Profession [0..]
allProfessions = [farmer, administrator, beggar, none]
allProfessionsVector = VB.fromList ([0,1,2,3] :: [Int])
professionToInt :: Profession -> Int
professionToInt (Profession p) = fromIntegral p
stringToProfession :: String -> Profession
stringToProfession a = case a of
	"farmer" -> farmer
	"administrator" -> administrator
	"beggar" -> beggar
	"none" -> none
	_ -> error "Incorrect profession name"

newtype ID = ID Int32 deriving (Show, Eq, Ord, Num)
{-# INLINE fromID #-}
fromID (ID id) = fromIntegral id
{-# INLINE toID #-}
toID id = ID $ fromIntegral id

instance Storable ID where
	sizeOf _ = sizeOf (undefined :: Int32)
	alignment _ = sizeOf (undefined :: Int32)
	peek p = liftM ID (peekElemOff (castPtr p :: Ptr Int32) 0)
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
	getId :: {-# UNPACK #-} !ID,
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
		dead_ <- peekElemOff q2 0
		gender_ <- peekElemOff q2 1
		profession_ <- peekElemOff q2 2
		culture_ <- peekElemOff q2 3

		let q = castPtr p :: Ptr Int32
		age_ <- peekElemOff q 1
		id_ <- peekElemOff q 2
		lover_ <- peekElemOff q 3
		parrents1 <- peekElemOff q 4
		parrents2 <- peekElemOff q 5
		position1 <- peekElemOff q 6
		position2 <- peekElemOff q 7
		return (Person dead_ (Gender gender_) (Profession profession_) (Culture culture_) (fromIntegral age_) (fromIntegral id_) (fromIntegral lover_) (fromIntegral parrents1, fromIntegral parrents2) (fromIntegral position1, fromIntegral position2))

	{-# INLINE poke #-}
	poke p (Person dead_ gender_ profession_ culture_ age_ id_ lover_ (parrents1, parrents2) (position1, position2)) = do
		let q2 = castPtr p :: Ptr Word8
		pokeElemOff q2 0 dead_
		pokeElemOff q2 1 $ fromGender gender_
		pokeElemOff q2 2 $ fromProfession profession_
		pokeElemOff q2 3 $ fromCulture culture_

		let q = castPtr p :: Ptr Int32
		pokeElemOff q 1 (age_ :: Int32)
		pokeElemOff q 2 (fromID id_ :: Int32)
		pokeElemOff q 3 (fromID lover_ :: Int32)
		pokeElemOff q 4 (fromID parrents1 :: Int32)
		pokeElemOff q 5 (fromID parrents2 :: Int32)
		pokeElemOff q 6 (position1 :: Int32)
		pokeElemOff q 7 (position2 :: Int32)


instance Eq Person where
	p1 == p2 = getId p1 == getId p2

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

