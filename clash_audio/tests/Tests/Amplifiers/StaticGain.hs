{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Amplifiers.GainTest where

import Amplifiers.StaticGain
import Clash.Explicit.Testbench
import Clash.Prelude

-- | Testbench to verify the 'gain' function with fixed gain factor.
tb :: Signal System Bool
tb = done
  where
    testInput = stimuliGenerator clk rst (1 :> 2 :> 3 :> 4 :> Nil)

    expectedOutput = outputVerifier' clk rst (2 :> 4 :> 6 :> 8 :> Nil)

    done = expectedOutput (topEntity clk rst en testInput)

    en = enableGen
    clk = tbSystemClockGen (pure False)
    rst = systemResetGen
