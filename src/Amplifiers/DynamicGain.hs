{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Amplifiers.DynamicGain
  ( gain,
    topEntity,
  )
where

import Clash.Prelude

-- | Applies a dynamic gain factor to an input audio signal.
--
-- @
--   factor: The gain multiplier as a signal.
--   input: The input audio signal.
--   output: The output audio signal.
-- @
gain ::
  (HiddenClockResetEnable dom, Num a, NFDataX a) =>
  Signal dom a ->
  Signal dom a ->
  Signal dom a
gain factor input = (*) <$> factor <*> input

-- | Top entity for FPGA synthesis with dynamic gain factor.
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  (Signal System (Signed 16), Signal System (Signed 16)) ->
  Signal System (Signed 16)
topEntity clk rst en (gainFactor, input) =
  withClockResetEnable clk rst en $ gain gainFactor input
{-# NOINLINE topEntity #-}
