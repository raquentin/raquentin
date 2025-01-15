{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.DynamicGainTest where

import Amplifiers.DynamicGain
import Clash.Explicit.Testbench
import Clash.Prelude

-- | Testbench to verify the 'gainDynamic' function with dynamic gain factor.
tb :: Signal System Bool
tb = done
  where
    testInputFactor = stimuliGenerator clk rst (2 :> 3 :> 4 :> 1 :> Nil)

    testInputSignal = stimuliGenerator clk rst (1 :> 2 :> 3 :> 4 :> Nil)

    expectedOutput = outputVerifier' clk rst (2 :> 6 :> 12 :> 4 :> Nil)

    done = expectedOutput (topEntity clk rst en (testInputFactor, testInputSignal))

    en = enableGen
    clk = tbSystemClockGen (pure False)
    rst = systemResetGen
