{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- |
-- Module    : Statistics.Transform
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Fourier-related transformations of mathematical functions.
--
-- These functions are written for simplicity and correctness, not
-- speed.  If you need a fast FFT implementation for your application,
-- you should strongly consider using a library of FFTW bindings
-- instead.

module Transform ( kPointFFT ) where

import Control.Monad         (when)
import Control.Monad.ST      (ST)
import Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import Data.Complex          (Complex(..), conjugate, realPart, magnitude)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U


type CD = Complex Float

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = error "Statistics.Math.log2: invalid input"
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [0x2, 0xc, 0xf0, 0xff00, 0xffff0000, 0xffffffff00000000]
    !sv = U.fromList [1,2,4,8,16,32]

hanning :: Float -> Float -> Float
hanning m n = 0.5 - 0.5 * cos(2 * pi * n / m)

kPointFFT :: U.Vector Float -> U.Vector Float
kPointFFT !vec = mag where
    winlen = realToFrac $ G.length vec - 1
    window = G.imap (\i x -> ((hanning winlen (realToFrac i)) * x) :+ 0.0) vec
    fftres = halfFFT window
    mag    = G.map (\x -> let val = 20 * (logBase 10 $ magnitude x) in if(val < 0) then 0 else val) fftres


halfFFT :: U.Vector CD -> U.Vector CD
halfFFT !vec = U.take (U.length vec `div` 2) (fft vec)

-- | Radix-2 decimation-in-time fast Fourier transform.
fft :: U.Vector CD -> U.Vector CD
fft !v = G.create $ do
          mv <- G.thaw v
          mfft mv
          return mv

mfft :: (M.MVector v CD) => v s CD -> ST s ()
mfft vec
    | 1 `shiftL` m /= len = error "Statistics.Transform.fft: bad vector size"
    | otherwise           = bitReverse 0 0
 where
  bitReverse i j | i == len-1 = stage 0 1
                 | otherwise  = do
    when (i < j) $ M.swap vec i j
    let inner k l | k <= l    = inner (k `shiftR` 1) (l-k)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 | l == m    = return ()
              | otherwise = do
    let !l2 = l1 `shiftL` 1
        !e  = -6.283185307179586/fromIntegral l2
        flight j !a | j == l1   = stage (l+1) l2
                    | otherwise = do
          let butterfly i | i >= len  = flight (j+1) (a+e)
                          | otherwise = do
                let i1 = i + l1
                xi1 :+ yi1 <- M.read vec i1
                let !c = cos a
                    !s = sin a
                    d  = (c*xi1 - s*yi1) :+ (s*xi1 + c*yi1)
                ci <- M.read vec i
                M.write vec i1 (ci - d)
                M.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
    flight 0 0
  len = M.length vec
  m   = log2 len

fi :: Int -> CD
fi = fromIntegral

halve :: Int -> Int
halve = (`shiftR` 1)