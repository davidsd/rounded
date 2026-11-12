{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Time per operation, in nanoseconds, of the basic operations ('*', '+', a
-- dot-product term @acc + x * y@, '/', '<'), against the safe path that
-- 'binary'', 'binary' and 'cmp' implement (what the same operations cost before
-- they made one unsafe call each).
--
-- Usage: rounded-ops-bench [PRECISION ...]   (default: 768 832 896 1024)
--
-- Each figure is the median of five runs over 1000 operands, rounding toward
-- zero.  Run it on an otherwise idle core.
----------------------------------------------------------------------------
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import qualified Data.List as List
import Data.Proxy (Proxy(..))
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import Text.Printf (printf)

import Numeric.MPFR.Raw
import Numeric.Rounded
import Numeric.Rounded.Internal

main :: IO ()
main = do
  args <- getArgs
  let precs = if null args then [768, 832, 896, 1024] else map read args
  printf "%6s  %-10s %10s %10s %8s\n" "bits" "operation" "safe (ns)" "fast (ns)" "speedup"
  forM_ precs $ \p -> reifyPrecision p $ \(_ :: Proxy p) -> do
    let xs = operands 1 :: [Rounded 'TowardZero p]
        ys = operands 2
        zs = operands 3
    _ <- evaluate (forceList xs + forceList ys + forceList zs)
    let row name safe fast = do
          ts <- time safe
          tf <- time fast
          printf "%6d  %-10s %10.1f %10.1f %7.2fx\n" p (name :: String) ts tf (ts / tf)
        -- Each repetition uses one of two operand lists, chosen at run time,
        -- so that no result can be shared between repetitions.
        elementwise f r = forceList (zipWith f xs (pick r ys zs) `asTypeOf` xs)
        dot add mul r = List.foldl' (\acc (x, y) -> acc `add` (x `mul` y)) 0 (zip xs (pick r ys zs))
        cmpCount less r = length (filter id (zipWith less xs (pick r ys zs)))
    row "mul" (elementwise (binary' mpfr_mul)) (elementwise (*))
    row "add" (elementwise (binary' mpfr_add)) (elementwise (+))
    row "dot term" (dot (binary' mpfr_add) (binary' mpfr_mul)) (dot (+) (*))
    row "div" (elementwise (binary mpfr_div)) (elementwise (/))
    row "compare" (cmpCount (cmp mpfr_less_p)) (cmpCount (<))

-- | Median time per operand, in nanoseconds, of five runs of 'reps' repetitions.
time :: (Int -> a) -> IO Double
time f = do
  ts <- forM [1 .. 5 :: Int] $ \_ -> do
    t0 <- getMonotonicTimeNSec
    forM_ [1 .. reps] $ \r -> evaluate (f r)
    t1 <- getMonotonicTimeNSec
    return (fromIntegral (t1 - t0) / fromIntegral (reps * n))
  return (List.sort ts !! 2)

n, reps :: Int
n = 1000
reps = 200

pick :: Int -> a -> a -> a
pick r a b = if r < 0 then a else b
{-# NOINLINE pick #-}

-- | Forces every element, and returns a value that depends on all of them.
forceList :: [Rounded r p] -> Int
forceList = List.foldl' (\ !acc x -> x `seq` acc + 1) 0

-- | Deterministic operands in (-2, 2), with all of their bits set by a
--   correctly rounded division.
operands :: forall r p. (Rounding r, Precision p) => Int -> [Rounded r p]
operands s = [ binary mpfr_div (fromInt (k * 7919 + s * 104729 - 5000000) :: Rounded r p)
                               (fromInt (k * 2477 + s + 2500000) :: Rounded r p)
             | k <- [1 .. n] ]
