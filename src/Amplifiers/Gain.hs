{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Amplifiers.Gain
  ( gain,
  )
where

import Clash.Prelude

-- |
--  Applies a gain factor to an input audio signal.
--
--  @
--    gainFactor: The gain multiplier (2.0 doubles the amplitude).
--    input: The input audio signal.
--    output: The output audio signal.
--  @
gain :: forall dom a. (HiddenClockResetEnable dom, Num a) => a -> Signal dom a -> Signal dom a
gain factor input = output
  where
    output = (* factor) <$> input
