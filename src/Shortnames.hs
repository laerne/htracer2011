module Shortnames
    where

-- | Short name to cast from Integers (usually to Ints) :
-- 
-- @
-- i_ = 'fromInteger'
-- @
i_:: (Num a) => Integer -> a
i_ = fromInteger

-- | Short name to cast to Integers (usually from Ints) :
-- 
-- @
-- i_ = 'toInteger'
-- @
_i:: Integral a => a -> Integer
_i = toInteger

-- | Short name to cast to Float. we us realToFrac for that.
-- 
-- @
-- _f = realToFrac
-- @
_f :: (RealFloat a) => a -> Float
_f = realToFrac


ii :: (Integral a, Num b) => a -> b
ii = fromIntegral


i_orf :: (Integral i, RealFloat a) => i -> a
i_orf = fromIntegral



