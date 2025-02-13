{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Top where

import Debug.Trace

import Clash.Prelude
import Data.Proxy (Proxy (..))
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols
import Protocols.Axi4.Stream

import qualified Data.Maybe as Maybe

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs =
      [ PortName "clk"
      , PortName "rst"
      , PortProduct ""
        [ PortName "rx_tvalid"
        , PortName "tx_tready"
        ]
      ]
    , t_output =
      PortProduct ""
        [ PortName "rx_tready"
        , PortName "tx_tvalid"
        ]
      }) #-}

type AxiDataWidth = 8
type AxisConfig = 'Axi4StreamConfig (AxiDataWidth `Div` 8) 0 0

type AxiSTopEntity dom cfg =
  Clock dom
  -> Reset dom
  -> (Signal dom (Maybe (Axi4StreamM2S cfg Bool)), Signal dom Axi4StreamS2M)
  -> (Signal dom Axi4StreamS2M, Signal dom (Maybe (Axi4StreamM2S cfg Bool)))

-- topEntity :: AxiSTopEntity System AxisConfig
topEntity clk rst = withClockResetEnable clk rst enableGen $ toSignals (ethAxisCircuit @System @())

myCompander ::
  forall dom i o s.
  (HiddenClockResetEnable dom, NFDataX s) =>
  s ->
  -- | Return `True` when you're finished with the current input value
  -- and are ready for the next one.
  -- Return `Just` to send the produced value off to the right.
  (s -> i -> (s, Maybe o, Bool)) ->
  Circuit (Df.Df dom i) (Df.Df dom o)
myCompander s0 f = Circuit (unbundle . go . bundle)
 where
  go :: Signal dom (Df.Data i, Ack) -> Signal dom (Ack, Df.Data o)
  go = mealy f' s0
  f' :: s -> (Df.Data i, Ack) -> (s, (Ack, Df.Data o))
  f' s (Df.NoData, _) = (s, (Ack False, Df.NoData))
  f' s (Df.Data i, Ack ack) = (s'', (Ack ackBack, maybe Df.NoData Df.Data o))
   where
    (s', o, doneWithInput) = f s i
    -- We only care about the downstream ack if we're sending them something
    mustWaitForAck = Maybe.isJust o
    (s'', ackBack) = if mustWaitForAck && not ack then (s, False) else (s', doneWithInput)

ethAxisCircuit
  :: forall dom a. _
  => Circuit
      (Df dom a)
      (Df dom a)
ethAxisCircuit =
  let
    asAxi = axiPassthrough
  in
    asAxi

axiPassthrough
    :: forall dom a. _
    => Circuit
      (Df dom a)
      (Df dom a)
axiPassthrough = Df.compander () (\_ raw -> ((), Just raw, True))

fromAxi
  :: Circuit
    (Axi4Stream dom cfg Bool)
    (Df dom (Axi4StreamM2S cfg Bool))
fromAxi =
  let
    go (Nothing, _) = (Axi4StreamS2M { _tready = False }, Df.NoData)
    go (Just axiM2S, Ack ack) = (Axi4StreamS2M { _tready = ack }, Df.Data axiM2S)
  in Circuit (unbundle . fmap go . bundle)

toAxi
  :: Circuit
    (Df dom (Axi4StreamM2S cfg Bool))
    (Axi4Stream dom cfg Bool)
toAxi =
  let
    go (Df.NoData, _) = (Ack False, Nothing)
    go (Df.Data m2s, s2m) = (Ack $ _tready s2m, Just m2s)
  in Circuit (unbundle . fmap go . bundle)
