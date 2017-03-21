{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded.Rounding
-- Copyright   :  (C) 2012-2014 Edward Kmett
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Various rounding modes
----------------------------------------------------------------------------
module Numeric.Rounded.Rounding
  ( Rounding(..)
  , RoundingMode(..)
  , reifyRounding
  ) where

import Data.Data
import Data.Singletons

data RoundingMode
  = TowardNearestWithTiesAwayFromZero -- ^ currently unsupported placeholder
  | TowardNearest -- ^ roundTiesToEven in IEEE 754-2008
  | TowardZero    -- ^ roundTowardZero in IEEE 754-2008
  | TowardInf     -- ^ roundTowardPositive in IEEE 754-2008
  | TowardNegInf  -- ^ roundTowardNegative in IEEE 754-2008
  | AwayFromZero  -- ^ round away from zero
  | Faithfully    -- ^ currently unsupported placeholder
  deriving (Eq,Ord,Show,Read,Data,Typeable)

class Rounding (r :: RoundingMode) where rounding :: Proxy r -> RoundingMode
instance Rounding TowardNearest where rounding _ = TowardNearest
instance Rounding TowardZero    where rounding _ = TowardZero
instance Rounding TowardInf     where rounding _ = TowardInf
instance Rounding TowardNegInf  where rounding _ = TowardNegInf
instance Rounding AwayFromZero  where rounding _ = AwayFromZero
instance Rounding Faithfully    where rounding _ = Faithfully
instance Rounding TowardNearestWithTiesAwayFromZero where rounding _ = TowardNearestWithTiesAwayFromZero

instance Enum RoundingMode where
  toEnum (-1) = TowardNearestWithTiesAwayFromZero
  toEnum 0 = TowardNearest
  toEnum 1 = TowardZero
  toEnum 2 = TowardInf
  toEnum 3 = TowardNegInf
  toEnum 4 = AwayFromZero
  toEnum 5 = Faithfully
  toEnum _ = error "out of range"

  fromEnum TowardNearestWithTiesAwayFromZero = -1
  fromEnum TowardNearest = 0
  fromEnum TowardZero = 1
  fromEnum TowardInf = 2
  fromEnum TowardNegInf = 3
  fromEnum AwayFromZero = 4
  fromEnum Faithfully = 5

instance Bounded RoundingMode where
  minBound = TowardNearestWithTiesAwayFromZero
  maxBound = Faithfully

newtype instance Sing (m :: RoundingMode) = SRounding RoundingMode

instance SingI TowardNearestWithTiesAwayFromZero where sing = SRounding TowardNearestWithTiesAwayFromZero
instance SingI TowardNearest where sing = SRounding TowardNearest
instance SingI TowardZero    where sing = SRounding TowardZero
instance SingI TowardInf     where sing = SRounding TowardInf
instance SingI TowardNegInf  where sing = SRounding TowardNegInf
instance SingI AwayFromZero  where sing = SRounding AwayFromZero
instance SingI Faithfully    where sing = SRounding Faithfully

reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> r) -> r
reifyRounding TowardNearestWithTiesAwayFromZero f = f (Proxy :: Proxy TowardNearestWithTiesAwayFromZero)
reifyRounding TowardNearest                     f = f (Proxy :: Proxy TowardNearest)
reifyRounding TowardZero                        f = f (Proxy :: Proxy TowardZero)
reifyRounding TowardInf                         f = f (Proxy :: Proxy TowardInf)
reifyRounding TowardNegInf                      f = f (Proxy :: Proxy TowardNegInf)
reifyRounding AwayFromZero                      f = f (Proxy :: Proxy AwayFromZero)
reifyRounding Faithfully                        f = f (Proxy :: Proxy Faithfully)
{-# INLINE reifyRounding #-}
