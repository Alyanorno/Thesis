{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Stuff where

import Prelude hiding (id)

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Data.Int
import System.Random.Mersenne.Pure64 as R
import Control.Monad.ST

import Definitions



{-# INLINE (.&&.) #-}
(.&&.) f g !a = f a && g a
infixr 4 .&&.
{-# INLINE (.||.) #-}
(.||.) f g !a = f a || g a
infixr 4 .||.

{-# INLINE (!) #-}
(!) v i = V.unsafeIndex v $ fromIntegral i

{-# INLINE (.!) #-}
(.!) :: (Integral i) => VB.Vector a -> i -> a
(.!) v i = VB.unsafeIndex v $ fromIntegral i

{-# INLINE (&!) #-}
(&!) :: People -> ID -> Person
(&!) people i = V.unsafeIndex people $ fromID (i - id (V.head people))

safeAccess :: People -> ID -> Maybe Person
safeAccess people i = if (ix < 0) || (ix > V.length people) then Nothing else Just (people ! ix)
	where ix = fromID $ i - id (V.head people)



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
rescale maxX maxY a = floor (fromIntegral a * ((fromIntegral maxY :: Float) / (fromIntegral maxX :: Float)))

--rescale_ :: Int -> Float -> Int -> Float
rescale_ maxX maxY a = fromIntegral a * maxY / fromIntegral maxX


start :: (Int32, Int32) -> Int -> People
start startPosition a = let off = a * 3 in V.fromList $ replicate (fromIntegral off) (Person 1 male farmer endorphi 70 1 0 (0,0) (0,0)) ++ [Person 0 g prof cult age (toID i) (if i >= a then 0 else 1) (if i >= a then (toID (1+i-a), toID (1+i-a)) else (ID 0,ID 0)) startPosition | (i,(g,(prof,cult))) <- zip [off+1..off+1+a*2] $ zip (cycle [male, female]) $ zip (infinitly allProfessions) (infinitly allCultures), let age = fromIntegral $ f (i-off) a]
	where
		f :: Int -> Int -> Int
		f i max
			| i >= floor ((fromIntegral max :: Float) * 0.75) = 0
			| i >= floor ((fromIntegral max :: Float) * 0.30) = 20
			| i >= floor ((fromIntegral max :: Float) * 0.10) = 40
			| otherwise = 60
		infinitly x = cycle $ concat $ zipWith (\a b -> [a, b]) x x

distanceTo :: (Float,Float) -> (Float,Float) -> Float
distanceTo (x,y) (x',y') = sqrt $ (x-x')^2 + (y-y')^2

toFloat :: (Integral i) => (i,i) -> (Float,Float)
toFloat (x,y) = (fromIntegral x, fromIntegral y)

impossiblePos :: Float
impossiblePos = 1000000

boxFilter :: Vector Float -> Vector Float
boxFilter list = V.imap (\i a -> let f = access a in (a + f (i-1) + f (i+1) + f (i - mapRange) + f (i + mapRange) / 5)) list
	where
		access a i = if i < 0 || i >= V.length list then a else list ! i
		mapRange = round $ sqrt $ fromIntegral $ V.length list

