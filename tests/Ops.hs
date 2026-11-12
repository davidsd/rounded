{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Checks that the basic operations, which make one unsafe foreign call each
-- (cbits/ops.c), give bit-for-bit the same results as the safe path that
-- 'binary', 'binary'', 'unary''' and 'cmp' implement, for random operands and
-- special values, at many precisions and in every rounding mode.
--
-- Two results are the same when their precision, sign and exponent fields are
-- equal and, for a regular number, their limbs are too.  (The safe path leaves
-- the limbs of a zero, infinity or NaN uninitialised; the fast path zeroes
-- them, and that is checked too.)
--
-- The checks run in several threads with a small allocation area, so that
-- garbage collections move the operands' arrays often between calls.
----------------------------------------------------------------------------
module Main (main) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List
import Data.Proxy (Proxy(..))
import Data.Word (Word64)
import GHC.Exts (ByteArray#, Int(..), indexWord8Array#, sizeofByteArray#)
import GHC.Word (Word8(..))
import System.Exit (exitFailure)

import Numeric.MPFR.Raw
import Numeric.Rounded
import Numeric.Rounded.Internal

-- Precisions in bits: the smallest ones, around a limb, the range the package
-- is used at in practice, and both sides of 'fastPrecMax'.
precisions :: [Int]
precisions =
  [1, 2, 3, 24, 53, 63, 64, 65, 127, 128, 129, 200, 512, 768, 832, 896, 1000, 1024, 2048, 4096, 16383, 16384, 16385, 20000]

-- Pairs of operands per precision and rounding mode.
pairsAt :: Int -> Int
pairsAt p
  | p <= 1024 = 1500
  | p <= 4096 = 400
  | otherwise = 60

roundings :: [RoundingMode]
roundings = [TowardNearest, TowardZero, TowardInf, TowardNegInf, AwayFromZero, Faithfully]

main :: IO ()
main = do
  failures <- newIORef ([] :: [String], [] :: [String])
  checks <- newIORef (0 :: Int)
  let record ok what msg = do
        atomicModifyIORef' checks (\n -> (n + 1, ()))
        unless ok $ atomicModifyIORef' failures $ \(whats, msgs) ->
          ((what : whats, if length msgs < 10 then msg : msgs else msgs), ())
  dones <- forM (zip [0 ..] precisions) $ \(i, p) -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      r <- try $ mapM_ (\(j, m) -> reifyRounding m $ \pr -> checkAt record pr p (seed i j)) (zip [0 ..] roundings)
      case r of
        Left (e :: SomeException) -> record False ("exception at " ++ show p) (show e)
        Right () -> return ()
      putMVar done ()
    return done
  mapM_ takeMVar dones
  -- Mixed precisions: the operands and the result each have their own.
  let mixed = [(53, 1024, 768), (1024, 53, 768), (768, 768, 64), (65, 129, 1000), (1000, 20000, 1000), (16384, 16384, 16385)]
  mapM_ (\(k, (p1, p2, p3)) -> reifyRounding TowardZero $ \pr -> checkMixed record pr p1 p2 p3 (seed 100 k))
        (zip [0 ..] mixed)
  mapM_ (\(k, (p1, p2, p3)) -> reifyRounding TowardInf $ \pr -> checkMixed record pr p1 p2 p3 (seed 200 k))
        (zip [0 ..] mixed)
  n <- readIORef checks
  (whats, msgs) <- readIORef failures
  putStrLn (show n ++ " checks, " ++ show (length whats) ++ " failures")
  mapM_ (\g -> putStrLn ("  " ++ show (length g) ++ " failures of " ++ concat (take 1 g))) (List.group (List.sort whats))
  unless (null msgs) $ putStrLn "The first failures:"
  mapM_ (\m -> putStrLn ("  " ++ take 300 m)) (reverse msgs)
  when (not (null whats) || n == 0) exitFailure
  where
    seed i j = 0x2545f4914f6cdd1d * fromIntegral (i * 16 + j + 1 :: Int)

-- | Record a check: whether it passed, what it checked, and a description.
type Record = Bool -> String -> String -> IO ()

-- | All operations at one precision and rounding mode.
checkAt :: forall r. Rounding r => Record -> Proxy r -> Int -> Word64 -> IO ()
checkAt record _ p s0 = reifyPrecision p $ \(_ :: Proxy p) -> do
  let pairs = operandPairs (pairsAt p) s0 :: [(Rounded r p, Rounded r p)]
      what name = name ++ " at " ++ show p ++ " bits, " ++ show (rounding (Proxy :: Proxy r))
      tag name a b = what name ++ ": " ++ show a ++ ", " ++ show b
      fast = p <= fromIntegral fastPrecMax
      result name new ref a b = record (same new ref && (not fast || zeroed new)) (what name) (tag name a b)
      bool name new ref a b = record (new == ref) (what name) (tag name a b)
  mapM_ (\(a, b) -> do
    result "+" (a + b) (binary' mpfr_add a b) a b
    result "-" (a - b) (binary' mpfr_sub a b) a b
    result "*" (a * b) (binary' mpfr_mul a b) a b
    result "/" (a / b) (binary mpfr_div a b) a b
    result ".+." (a .+. b) (binary' mpfr_add a b) a b
    result ".-." (a .-. b) (binary' mpfr_sub a b) a b
    result ".*." (a .*. b) (binary' mpfr_mul a b) a b
    result "min" (min a b) (binary' mpfr_min a b) a b
    result "max" (max a b) (binary' mpfr_max a b) a b
    result "negate" (negate a) (unary'' mpfr_neg a) a b
    result "abs" (abs a) (unary'' mpfr_abs a) a b
    result "negate'" (negate' b) (unary'' mpfr_neg b) a b
    result "abs'" (abs' b) (unary'' mpfr_abs b) a b
    bool "==" (a == b) (cmp mpfr_equal_p a b) a b
    bool "/=" (a /= b) (cmp mpfr_lessgreater_p a b) a b
    bool "<=" (a <= b) (cmp mpfr_lessequal_p a b) a b
    bool "<" (a < b) (cmp mpfr_less_p a b) a b
    bool ">=" (a >= b) (cmp mpfr_greaterequal_p a b) a b
    bool ">" (a > b) (cmp mpfr_greater_p a b) a b
    bool "compare" (compare a b) (compare (cmp' mpfr_cmp a b) 0) a b
    bool "!<>!" (a !<>! b) (cmp mpfr_unordered_p a b) a b
    ) pairs
  -- Chains: a dot product and a Horner evaluation, the same steps by both paths.
  let xs = map fst pairs
      ys = map snd pairs
      dot = List.foldl' (+) 0 (zipWith (*) xs ys)
      dotRef = List.foldl' (binary' mpfr_add) 0 (zipWith (binary' mpfr_mul) xs ys)
      x0 = binary' mpfr_div (fromDouble 1) (fromDouble 3)
      horner = List.foldl' (\acc c -> acc * x0 + c) 0 xs
      hornerRef = List.foldl' (\acc c -> binary' mpfr_add (binary' mpfr_mul acc x0) c) 0 xs
  result "dot product" dot dotRef dot dotRef
  result "Horner" horner hornerRef horner hornerRef

-- | The mixed-precision operations, with operands at p1 and p2 and the result at p3.
checkMixed :: forall r. Rounding r => Record -> Proxy r -> Int -> Int -> Int -> Word64 -> IO ()
checkMixed record _ p1 p2 p3 s0 =
  reifyPrecision p1 $ \(_ :: Proxy p1) -> reifyPrecision p2 $ \(_ :: Proxy p2) -> reifyPrecision p3 $ \(_ :: Proxy p3) -> do
  let as = map fst (operandPairs 300 s0) :: [Rounded r p1]
      bs = map snd (operandPairs 300 (s0 + 1)) :: [Rounded r p2]
      what name = name ++ " at " ++ show (p1, p2, p3) ++ " bits, " ++ show (rounding (Proxy :: Proxy r))
      tag name a b = what name ++ ": " ++ show a ++ ", " ++ show b
      fast = maximum [p1, p2, p3] <= fromIntegral fastPrecMax
      result name new ref a b = record (same new ref && (not fast || zeroed new)) (what name) (tag name a b)
      bool name new ref a b = record (new == ref) (what name) (tag name a b)
  mapM_ (\(a, b) -> do
    result "add_" (add_ a b :: Rounded r p3) (binary mpfr_add a b) a b
    result "sub_" (sub_ a b :: Rounded r p3) (binary mpfr_sub a b) a b
    result "mul_" (mul_ a b :: Rounded r p3) (binary mpfr_mul a b) a b
    result "div_" (div_ a b :: Rounded r p3) (binary mpfr_div a b) a b
    result "!+!" (a !+! b :: Rounded r p3) (binary mpfr_add a b) a b
    result "!/!" (a !/! b :: Rounded r p3) (binary mpfr_div a b) a b
    result "min_" (min_ a b :: Rounded r p3) (binary mpfr_min a b) a b
    result "max_" (max_ a b :: Rounded r p3) (binary mpfr_max a b) a b
    bool "!==!" (a !==! b) (cmp mpfr_equal_p a b) a b
    bool "!/=!" (a !/=! b) (cmp mpfr_lessgreater_p a b) a b
    bool "!<=!" (a !<=! b) (cmp mpfr_lessequal_p a b) a b
    bool "!<!" (a !<! b) (cmp mpfr_less_p a b) a b
    bool "!>=!" (a !>=! b) (cmp mpfr_greaterequal_p a b) a b
    bool "!>!" (a !>! b) (cmp mpfr_greater_p a b) a b
    bool "!<>!" (a !<>! b) (cmp mpfr_unordered_p a b) a b
    bool "compare_" (compare_ a b) (compare (cmp' mpfr_cmp a b) 0) a b
    ) (zip as bs)

-- | Bit-for-bit equality of the fields that determine a value.
same :: Rounded r p -> Rounded r p -> Bool
same x@(Rounded px sx ex lx) (Rounded py sy ey ly) =
  px == py && sx == sy && ex == ey && (not (isRegular x) || bytes lx == bytes ly)

-- | A zero, infinity or NaN has zeroed limbs (true of the fast path only).
zeroed :: Rounded r p -> Bool
zeroed x@(Rounded _ _ _ l) = isRegular x || all (== 0) (bytes l)

isRegular :: Rounded r p -> Bool
isRegular = tst mpfr_regular_p

bytes :: ByteArray# -> [Word8]
bytes l = [W8# (indexWord8Array# l i) | I# i <- [0 .. I# (sizeofByteArray# l) - 1]]

-- Operands

-- | Pairs of operands: mostly random regular numbers, with exponents close
--   enough together that additions round and cancel, plus equal, opposite and
--   nearly equal pairs, numbers near the exponent range's ends (to overflow and
--   underflow), and the special values.
operandPairs :: (Rounding r, Precision p) => Int -> Word64 -> [(Rounded r p, Rounded r p)]
operandPairs n s0 = take n (go (randoms s0))
  where
    go (w : ws) = case w `mod` 16 of
      0 -> let (a, ws') = special ws; (b, ws'') = anyValue ws' in (a, b) : go ws''
      1 -> let (a, ws') = anyValue ws; (b, ws'') = special ws' in (a, b) : go ws''
      2 -> let (a, ws') = regular ws in (a, a) : go ws'
      3 -> let (a, ws') = regular ws in (a, unary'' mpfr_neg a) : go ws'
      4 -> let (a, ws') = regular ws; (b, ws'') = near a ws' in (a, b) : go ws''
      5 -> let (a, ws') = extreme ws; (b, ws'') = extreme ws' in (a, b) : go ws''
      _ -> let (a, ws') = regular ws; (b, ws'') = regular ws' in (a, b) : go ws''
    go [] = []
    anyValue ws@(w : _) | w `mod` 4 == 0 = special (drop 1 ws)
    anyValue ws = regular ws

-- | A random regular number with all of its bits random.
regular :: forall r p. (Rounding r, Precision p) => [Word64] -> (Rounded r p, [Word64])
regular (w : ws) = (encodeFloat m e, ws')
  where
    (m0, ws') = mantissa (precision (Proxy :: Proxy p)) ws
    m = if w .&. 1 == 0 then m0 else negate m0
    e = fromIntegral ((w `shiftR` 8) `mod` 81) - 40 - precision (Proxy :: Proxy p)
regular [] = error "regular"

-- | A random number whose exponent is within a few of the largest or smallest.
extreme :: forall r p. (Rounding r, Precision p) => [Word64] -> (Rounded r p, [Word64])
extreme (w : ws) = (encodeFloat m e, ws')
  where
    p = precision (Proxy :: Proxy p)
    (m, ws') = mantissa p ws
    (emin, emax) = floatRange (0 :: Rounded r p)
    d = fromIntegral ((w `shiftR` 8) `mod` 4)
    e = if w .&. 1 == 0 then emax - p - d else emin - p + 1 + d
extreme [] = error "extreme"

-- | A number that agrees with the given one except in its lowest bits.
near :: Rounded r p -> [Word64] -> (Rounded r p, [Word64])
near a (w : ws) = (iterate step a !! fromIntegral (w `mod` 4 + 1), ws)
  where step = if w .&. 16 == 0 then succUlp else predUlp
near a [] = (a, [])

-- | A random p-bit integer whose top bit is set.
mantissa :: Int -> [Word64] -> (Integer, [Word64])
mantissa p ws = (m .|. (1 `shiftL` (p - 1)), rest)
  where
    k = (p + 63) `div` 64
    (used, rest) = splitAt k ws
    m = List.foldl' (\acc w -> acc `shiftL` 64 .|. toInteger w) 0 used `shiftR` (64 * k - p)

-- | One of ±0, ±Inf and NaN, or ±1 (exact in every operation).
special :: (Rounding r, Precision p) => [Word64] -> (Rounded r p, [Word64])
special (w : ws) = (values !! fromIntegral (w `mod` 7), ws)
  where values = map fromDouble [0, -0, 1 / 0, -1 / 0, 0 / 0, 1, -1]
special [] = error "special"

-- | SplitMix64.
randoms :: Word64 -> [Word64]
randoms s = mix s' : randoms s'
  where
    s' = s + 0x9e3779b97f4a7c15
    mix z0 = let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
                 z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
             in z2 `xor` (z2 `shiftR` 31)
