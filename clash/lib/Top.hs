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

{-# ANN topEntity 
  (Synthesize
    { t_name   = "top"
    , t_inputs =
      [ PortName "clk"
      , PortName "rst"
      , PortProduct ""
        [ PortProduct "rx"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tkeep"
            , PortName "tstrb"
            , PortName "tlast"
            , PortName "tid"
            , PortName "tdest"
            , PortName "tuser"
            ]
          ]
        , PortName "tx_tready"
        ]
      ]
    , t_output =
      PortProduct ""
        [ PortName "rx_tready"
        , PortProduct "tx"
          [ PortName "tvalid"
          , PortProduct ""
            [ PortName "tdata"
            , PortName "tkeep"
            , PortName "tstrb"
            , PortName "tlast"
            , PortName "tid"
            , PortName "tdest"
            , PortName "tuser"]
          ]
        ]
      }) #-}

type AxiDataWidth = 64
type AxisConfig = 'Axi4StreamConfig (AxiDataWidth `Div` 8) 0 0

type AxiSTopEntity dom cfg =
  Clock dom 
  -> Reset dom 
  -> (Signal dom (Maybe (Axi4StreamM2S cfg Bool)), Signal dom Axi4StreamS2M) 
  -> (Signal dom Axi4StreamS2M, Signal dom (Maybe (Axi4StreamM2S cfg Bool)))

topEntity :: AxiSTopEntity System AxisConfig
topEntity clk rst = withClockResetEnable clk rst enableGen $ toSignals ethAxisCircuit

ethAxisCircuit 
  :: forall dom cfg. _
  => Circuit
      (Axi4Stream dom cfg Bool)
      (Axi4Stream dom cfg Bool)
ethAxisCircuit = 
  let
    proxy = Proxy @(Axi4Stream dom cfg Bool)
    fromDf = DfConv.dfToDfConvInp proxy
    toDf = DfConv.dfToDfConvOtp proxy
    asAxi = fromAxi |> axiPassthrough |> toAxi 
  in
    asAxi

axiPassthrough
    :: forall dom cfg. _
    => Circuit
      (Df dom (Axi4StreamM2S cfg Bool))
      (Df dom (Axi4StreamM2S cfg Bool))
axiPassthrough = Df.compander () (\() raw -> ((), Just raw, True))

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

