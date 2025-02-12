{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TestFuncs where

import qualified Prelude as P
import Data.Bits
import qualified Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Word
import Debug.Trace

import Clash.Prelude hiding (unconcatI)
import Clash.Prelude.Testbench
import Clash.Sized.BitVector
import qualified Clash.Sized.Vector as SV
import Clash.Class.BitPack

import Protocols
import Protocols.Axi4.Stream 

import Top

feedAxiSTopEntity 
  :: forall dom cfg n.
    ( HiddenClockResetEnable dom
    , KnownAxi4StreamConfig cfg
    , NFDataX (Axi4StreamM2S cfg Bool)
    , KnownNat n
    )
  => AxiSTopEntity dom cfg
  -> Vec n (Axi4StreamM2S cfg Bool)           -- ^ Data items we want to send
  -> Signal dom (Maybe (Axi4StreamM2S cfg Bool))
feedAxiSTopEntity topEntity inputM2S =
  let
    clk = clockGen
    rst = resetGen
    enable = enableGen
    mealyF :: Index (n+1) -> Axi4StreamS2M -> (Index (n+1), Maybe (Axi4StreamM2S cfg Bool))
    mealyF n s2m = case (n == maxBound, _tready s2m) of 
                    (True, _) -> (n, Nothing)
                    (False, True) -> (n+1, Just $ inputM2S !! n )
                    (False, False) -> (n, Just $ inputM2S !! n)
    mealyM = mealy mealyF 0
    withCRE = withClockResetEnable clk rst enable
    s2MInput = withCRE (traceShowId axiS2MAlwaysReady)
    (outS2M, outM2S) = topEntity
                        clk rst 
                        (mealyM outS2M, s2MInput)
  in outM2S

axiS2MAlwaysReady 
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom Axi4StreamS2M
axiS2MAlwaysReady = stimuliGenerator $ replicate d500 Axi4StreamS2M{ _tready = True }

topEntityRunner
  :: forall dom cfg n.
     ( HiddenClockResetEnable dom
     , NFDataX (Axi4StreamM2S cfg Bool)
     , KnownNat n
     )
  => AxiSTopEntity dom cfg
  -> Vec n (Axi4StreamM2S cfg Bool)  -- Data to input
  -> Unsigned 8
  -> Signal dom (Maybe (Axi4StreamM2S cfg Bool))
topEntityRunner theTopEntity inputVec waitTicks =
  let
    m2sGenerator :: Signal dom (Maybe (Axi4StreamM2S cfg Bool))
    m2sGenerator = axiM2SGenerator inputVec waitTicks outS2M

    s2MInput :: Signal dom Axi4StreamS2M
    s2MInput = pure $ Axi4StreamS2M True

    (outS2M, outM2S) = theTopEntity clockGen resetGen (m2sGenerator, s2MInput)
  in outM2S

axiM2SGenerator
  :: (HiddenClockResetEnable dom, NFDataX (Axi4StreamM2S cfg Bool), KnownNat n)
  => Vec n (Axi4StreamM2S cfg Bool)           -- Data to input
  -> Unsigned 8                               -- Number of ticks to wait
  -> Signal dom Axi4StreamS2M              -- the acks we get back
  -> Signal dom (Maybe (Axi4StreamM2S cfg Bool))
axiM2SGenerator initialQueue waitTicks =
  mealy axiStateMachine (initialQueue, waitTicks, 0)

axiStateMachine
  :: (KnownNat n)
  => (Vec n (Axi4StreamM2S cfg Bool), Unsigned 8, Index (n + 1))
  -> Axi4StreamS2M
  -> ((Vec n (Axi4StreamM2S cfg Bool), Unsigned 8, Index (n + 1)), Maybe (Axi4StreamM2S cfg Bool))
axiStateMachine state@(queue, waitTicks, n) s2mVal =
  let
    waitFinished = waitTicks == 0 
    finished = n == maxBound
    outFwd = if waitFinished && not finished then Just $ queue !! n else Nothing
    nextState = case (finished, waitFinished, _tready s2mVal) of 
                         (_, False, _) -> (queue, waitTicks - 1, n)
                         (False, True, True) -> (queue, waitTicks, n+1)
                         _ -> state
  in (nextState, outFwd)

simpleRoundTripTopEntity :: AxiSTopEntity System AxisConfig
simpleRoundTripTopEntity clk rst = withClockResetEnable clk rst enableGen $ toSignals circuitRoundTrip

circuitRoundTrip 
  :: forall dom cfg. (KnownAxi4StreamConfig cfg, HiddenClockResetEnable dom)
  => Circuit
      (Axi4Stream dom cfg Bool)
      (Axi4Stream dom cfg Bool)
circuitRoundTrip = 
  let
    proxy = Proxy @(Axi4Stream dom cfg Bool)
  in fromAxi |> toAxi 
