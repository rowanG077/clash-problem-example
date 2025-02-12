import qualified Data.List as L
import Data.Maybe as M
import Data.Proxy
import Debug.Trace
import Test.Hspec

import qualified Prelude as P

import Clash.Prelude as CP hiding (simulate, last, length)
import Clash.Prelude.Testbench
import qualified Clash.Signal.Internal as CSI
import qualified Clash.Signal (withClockResetEnable)
import qualified Clash.Sized.Vector as SV
import Protocols 
import Protocols.DfConv hiding (fst, catMaybes, sample)
import qualified Protocols.DfConv as DfConv
import Protocols.Axi4.Stream hiding (DataWidth)

-- Test modules
import TestFuncs

import Top

simConfig :: SimulationConfig
simConfig = SimulationConfig 0 100 False

type AxiCfg16 = 'Axi4StreamConfig 2 0 0
type AxiCfg32 = 'Axi4StreamConfig 4 0 0
type AxiCfg64 = 'Axi4StreamConfig 8 0 0
type AxiCfg128 = 'Axi4StreamConfig 16 0 0

main :: IO ()                                                                                                                                                                                                      
main = hspec $ do                                                                                                                                                                                                  
  describe "Top" $ do
    it "recreation of cocotb test" $ do
      let asAxis = Axi4StreamM2S {
        _tdata = 0x0B :> 0x0B :> 0x0B :> 0x0B :> 0x0B :> 0x0B :> 0x0A :> 0x0A  :> Nil
        , _tkeep = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tstrb = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Axi4StreamM2S {
        _tdata = 0x0A :> 0x0A :> 0x0A :> 0x0A  :> 0x0C :> 0x0C :> 0x45 :> 0x14 :> Nil
        , _tkeep = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tstrb = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Axi4StreamM2S {
        _tdata =  0x00 :> 0x15 :> 0x16 :> 0x16 :> 0x00 :> 0x00 :> 0x18 :> 0x19 :> Nil
        , _tkeep = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tstrb = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Axi4StreamM2S {
        _tdata = 0x1A :> 0x1A :> 0x1B :> 0x1B :> 0x1B :> 0x1B :> 0x1C :> 0x1C :> Nil
        , _tkeep = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tstrb = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Axi4StreamM2S {
        _tdata = 0x1C :> 0x1C :> 0x1E :> 0x1E :> 0x1F :> 0x1F :> 0x20 :> 0x20 :> Nil
        , _tkeep = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tstrb = True :> True :> True :> True :> True :> True :> True :> True :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Axi4StreamM2S {
        _tdata = 0x21 :> 0x21 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> 0x00 :> Nil
        , _tkeep = True :> True :> False :> False :> False :> False :> False :> False :> Nil
        , _tstrb = True :> True :> False :> False :> False :> False :> False :> False :> Nil
        , _tlast = False 
        , _tid = 0
        , _tdest = 0
        , _tuser = False
      } :> Nil
      let withCRE = withClockResetEnable systemClockGen systemResetGen enableGen
      let outM2s = withCRE $ topEntityRunner topEntity asAxis 2
      let sampled = sampleWithResetN d1 20 outM2s
      let sampled' = catMaybes (traceShowId sampled)
      sampled' `shouldBe` toList asAxis

