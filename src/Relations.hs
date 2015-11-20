{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DataKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Relations (createRelations) where

import Prelude hiding (id)
import GHC.Float

import Data.Vector.Storable (Vector)
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Mutable as MB
import qualified Data.Vector.Storable.Mutable as M

import Foreign.Storable.Tuple ()

import System.IO.Unsafe
import System.Random hiding (next, random, randoms)
import System.Random.Mersenne.Pure64 as R
import Control.Exception (assert)
--import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Primitive
import qualified Control.Monad.Parallel as P

import Definitions
import Stuff



createRelations :: Xorshift -> Friends -> People -> People -> VB.Vector (Vector ID) -> VB.Vector (Vector Float) -> VB.Vector (Vector ID) -> VB.Vector (Vector Float) -> (People, Friends)
createRelations gen friendss people alivePeople professionals professionalRelations culturals culturalRelations = unsafePerformIO $ do
	let s = V.length people
--	If fixed the result will not change depending on number of threads
	let offset = 12 --numCapabilities
	let randomGenerators = map (pureMT.fromIntegral) $ iterate (fromXorshift.step.Xorshift) (fromXorshift gen)
	let genWithRanges = zip randomGenerators $ let t = [0, s `div` offset..s-offset] ++ [s] in zip t (tail t)

--	f <- VB.unsafeThaw friendss
	f <- VB.thaw friendss
--	fRef <- newMVar f
	applyFriendMatches f genWithRanges
--	t1 <- takeMVar fRef
--	f' <- VB.unsafeFreeze f
	f' <- VB.freeze f

--	v <- V.unsafeThaw people
	v <- V.thaw people
--	vRef <- newMVar v
	applyLoveMatches v f' genWithRanges
--	t2 <- takeMVar vRef
--	v' <- V.unsafeFreeze v
	v' <- V.freeze v

	return (v', f')
	where
	applyFriendMatches :: MB.MVector RealWorld (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyFriendMatches fends list = P.mapM (\(gen_, (from, to)) -> applyMatches from to gen_) list
--	applyFriendMatches fends list = mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i end gen_
			| i >= end = return ()
			| (not.alive) person = applyMatches (i+1) end gen_
			| otherwise = do
--				withMVar fRef (\fends -> do
				friend <- MB.read fends i;
				if V.length friend + V.length m < maxFriends then do
					MB.write fends i (friend V.++ m);
				else do
					MB.write fends i ((V.drop (V.length m) friend) V.++ m );
				applyMatches (i+1) end gen_'
			where
			person = people ! i

			maxFriends = 100 + ((fromIntegral . fromXorshift . step . Xorshift . fromID. getId $ person) `rem` 200)

			-- Pick friends
			m :: Vector ID
			(m, gen_') = match i person gen_

	applyLoveMatches :: M.MVector RealWorld Person -> VB.Vector (Vector ID) -> [(PureMT, (Int, Int))] -> IO [()]
	applyLoveMatches v fends list = P.mapM (\(gen_, (from, to)) -> applyMatches from to gen_) list
--	applyLoveMatches vRef fends list = mapM (\(gen, (from, to)) -> applyMatches from to gen) list
		where
		applyMatches :: Int -> Int -> PureMT -> IO ()
		applyMatches i end gen_
			| i >= end = return ()
			| not . alive $ person = next
			| V.null friends = next
			| (==female) . gender .&&. (==0) . lover .&&. (<41) . age $ person = do
				when ((not.V.null) potentialLovers) $ do
					i' <- (return $ fromID $ m - start);

--					withMVar vRef (\v -> do
					p' <- M.read v i';
					p <- M.read v i;
					when (lover p' == 0 && lover p == 0) $ do
						M.write v i' (p' {lover = start + (toID i)});
						M.write v i (p {lover = m});
				next
			| otherwise = next
			where
			start = getId $ V.head people
			next = applyMatches (i+1) end gen_'
			person = people ! i
			friends = fends .! i

			potentialLovers :: Vector ID
			potentialLovers = V.filter (conditions . (people &!)) $ V.filter ((>=0) . (+(-start))) friends

			{-# INLINE conditions #-}
			conditions x = alive .&&. ((10<) .&&. (<50)) . age .&&. (==0) . lover .&&. ((/=) (parrents person)) . parrents $ x

			m = potentialLovers ! r
			(r, gen_') = randomR (0, V.length potentialLovers-1) gen_

	{-# INLINE match #-}
	match :: Int -> Person -> PureMT -> (Vector ID, PureMT)
	match personIndex person gen_
		| V.null potentialMates = (V.empty, gen_)
		| otherwise = (\(r, g)-> (V.map (potentialMates !) $ scale r (V.length potentialMates-1), g)) $ randomVector_ (timeStep*4) gen_'
		where
			potentialMates = potentialRandom V.++ potentialSameProfessional V.++ potentialSameCulture V.++ potentialProfessional V.++ potentialCulture V.++ potentialFriendsFriend

			(!random, gen_') = randomVector_ (timeStep*2) gen_

			scale :: Vector Float -> Int -> Vector Int
			scale v upper = V.map (floor.(* (int2Float upper))) v

			potentialRandom = V.map (getId . (alivePeople !)) $ scale (V.slice 0 timeStep random) (V.length alivePeople-1)
			potentialSameProfessional = V.map (prof !) $ scale random (V.length prof-1)
			potentialSameCulture = V.map (cult !) $ scale random (V.length cult-1)

			potentialProfessional :: Vector ID
			potentialProfessional = assert (V.sum profs <= V.length random) $ f 0 V.empty 0
				where
					f :: Int -> Vector ID -> Int -> Vector ID
					f i v offset
						| i >= V.length profs = v
						| V.null p || profs ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector ID
							result = V.map (p !) $ scale (V.slice offset (profs ! i) random) (V.length p-1)
							offset' = offset + profs ! i
							p = professionals .! i

			profs = V.map (floor . (* (int2Float $ timeStep*2))) $ normalize $ professionalRelations .! (professionToInt $ profession person)

			potentialCulture :: Vector ID
			potentialCulture = assert (V.sum cults <= V.length random) $ f 0 V.empty 0
				where
					f :: Int -> Vector ID -> Int -> Vector ID
					f i v offset
						| i >= V.length cults = v
						| V.null p || cults ! i <= 0 = v V.++ (f (i+1) v offset')
						| otherwise = v V.++ result V.++ (f (i+1) v offset')
						where
							result :: Vector ID
							result = V.map (p !) $ scale (V.slice offset (cults ! i) random) (V.length p-1)
							offset' = offset + cults ! i
							p = culturals .! i

			cults = V.map (floor.(* (int2Float $ timeStep*2))) $ normalize $ culturalRelations .! (cultureToInt $ culture person)

			normalize v = V.map (/ (V.sum v)) v

			prof :: Vector ID
			prof = professionals .! (professionToInt $ profession person)

			cult :: Vector ID
			cult = culturals .! (cultureToInt $ culture person)

			-- Pick friends from friends friends
			potentialFriendsFriend :: Vector ID
			potentialFriendsFriend
				| V.null friends = V.empty
				| otherwise = V.zipWith (\r friendID -> let ff = friendss .! (fromID $ friendID - start :: Int) in ff ! (floor $ r * (fromIntegral $ V.length ff-1) :: Int)) r2 $ V.filter (>= start) $ V.map (friends !) $ scale r1 (V.length friends-1)
				where
				start = getId $ V.head people

				r1 = V.slice 0 timeStep random
				r2 = V.slice timeStep timeStep random

				friends :: Vector ID
				friends = friendss .! personIndex

