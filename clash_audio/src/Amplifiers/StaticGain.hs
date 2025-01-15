{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Amplifiers.StaticGain
  ( gain,
    topEntity,
  )
where

import Clash.Prelude

-- | Applies a fixed gain factor to an input audio signal.
--
-- @
--   factor: The gain multiplier as a constant.
--   input: The input audio signal.
--   output: The output audio signal.
-- @
gain ::
  (HiddenClockResetEnable dom, Num a, NFDataX a) =>
  a ->
  Signal dom a ->
  Signal dom a
gain factor input = (* factor) <$> input

-- | Top entity for FPGA synthesis with fixed gain factor.
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Signed 16) ->
  Signal System (Signed 16)
topEntity = exposeClockResetEnable (gain 2)
{-# NOINLINE topEntity #-}
