-- |
-- Module      :  Numeric.Stats
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A very basic descriptive statistics. Functions use a tail recursion approach to compute the values and are strict by an accumulator.

{-# LANGUAGE BangPatterns, MagicHash #-}

module Numeric.Stats where

import GHC.Exts
import GHC.Prim

-- | Uses GHC unlifted types from @ghc-prim@ package.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
--
mean2F :: [Float] -> Float# -> Int# -> Float
mean2F ((F# !x):xs) !s1 !l1 = mean2F xs (plusFloat# s1 x) (l1 +# 1#)
mean2F _ !s1 !l1 =
 case I# l1 of
  0 -> error "Not defined for the third zero argument. "
  _  -> F# m
   where !m = divideFloat# s1 (int2Float# l1)

-- | Uses GHC unlifted types from @ghc-prim@ package.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
--
mean2D :: [Double] -> Double# -> Int# -> Double
mean2D ((D# !x):xs) !s1 !l1 = mean2D xs (s1 +## x) (l1 +# 1#)
mean2D _ !s1 !l1 =
 case I# l1 of
  0 -> error "Not defined for the third zero argument. "
  _  -> D# m
   where !m = s1 /## int2Double# l1

-- | One-pass and tail-recursive realization for the pair of the mean and dispersion. Is vulnerable to the floating-point cancellation errors.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
-- When using the needed, please, refer better to their variants.
--
-- Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersion :: (RealFrac a, Floating a) => [a] -> a -> a -> a -> a -> a -> (a,a)
meanWithDispersion (!x:xs) !s1 !s2 !l1 m1 d = meanWithDispersion xs (s1 + x) (s2 + x*x) (l1 + 1) (m0 s1 l1 x) (m0 s2 l1 (x*x) - (m0 s1 l1 x)**2)
  where m0 !s3 !l2 !x = (s3 + x) / (l2 + 1)
meanWithDispersion _ _ _ _ !m !d = (m,d)

-- | Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersionF :: [Float] -> Float# -> Float# -> Int# -> (Float,Float)
meanWithDispersionF ((F# !x):xs) !s1 !s2 !l1 = meanWithDispersionF xs (plusFloat# s1 x) (plusFloat# s2 (timesFloat# x x)) (l1 +# 1#)
meanWithDispersionF [] !s1 !s2 !l1 = (F# m, F# (minusFloat# (divideFloat# s2 (int2Float# l1)) (timesFloat# m m)))
  where !m = divideFloat# s1 (int2Float# l1)

-- | Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersionD :: [Double] -> Double# -> Double# -> Int# -> (Double,Double)
meanWithDispersionD ((D# !x):xs) !s1 !s2 !l1 = meanWithDispersionD xs (s1 +## x) (s2 +## (x *## x)) (l1 +# 1#)
meanWithDispersionD [] !s1 !s2 !l1 = (D# m, D# ((s2 /## int2Double# l1) -## (m *## m)))
  where !m = s1 /## int2Double# l1

-- | Uses 'mean2F' inside.
meanF :: [Float] -> Float
meanF xs = mean2F xs 0.0# 0#
{-# INLINE meanF #-}

meanD :: [Double] -> Double
meanD xs = mean2D xs 0.0## 0#

-- | Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDisp :: (RealFrac a, Floating a) => [a] -> (a,a)
meanWithDisp xs
 | null xs = error "Not defined for the empty list. "
 | otherwise = meanWithDispersion xs 0.0 0.0 0.0 0.0 0.0
{-# RULES "realfroc/float" meanWithDisp = meanWithDispF2 #-}
{-# RULES "realfroc/double" meanWithDisp = meanWithDispD2 #-}
{-# INLINE[2] meanWithDisp #-}

-- | Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDispF2 :: [Float] -> (Float,Float)
meanWithDispF2 xs
 | null xs = error "Not defined for the empty list. "
 | otherwise = meanWithDispersionF xs 0.0# 0.0# 0#
{-# INLINE meanWithDispF2 #-}

-- | Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDispD2 :: [Double] -> (Double,Double)
meanWithDispD2 xs
 | null xs = error "Not defined for the empty list. "
 | otherwise = meanWithDispersionD xs 0.0## 0.0## 0#
{-# INLINE meanWithDispD2 #-}
