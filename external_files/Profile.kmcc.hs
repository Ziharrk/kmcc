{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Mem as S
import qualified GHC.Stats as G
import BasicDefinitions

profiledotgetProcessInfos_Det# = do
  stats <- G.getRTSStats
  let gcs = G.gcs stats
      heap = G.max_live_bytes stats
      mem = G.allocated_bytes stats
      cpu = G.mutator_cpu_ns stats
      elapsed = G.mutator_elapsed_ns stats
  P.return P.$ CCons_Det (CTuple2_Det RunTime_Det (P.div (P.toInteger cpu) (10 P.^ 6)))
    P.$ CCons_Det (CTuple2_Det ElapsedTime_Det (P.div (P.toInteger elapsed) (10 P.^ 6)))
    P.$ CCons_Det (CTuple2_Det Memory_Det (P.toInteger mem))
    P.$ CCons_Det (CTuple2_Det Heap_Det (P.toInteger heap))
    P.$ CCons_Det (CTuple2_Det GarbageCollections_Det (P.toInteger heap)) CList_Det
profiledotgetProcessInfos_ND# = P.return P.$ P.fmap from profiledotgetProcessInfos_Det#

profiledotgarbageCollectorOff_Det# = P.error "No implementation of profiledotgarbageCollectorOff_Det#"
profiledotgarbageCollectorOff_ND# = P.error "No implementation of profiledotgarbageCollectorOff_ND#"

profiledotgarbageCollectorOn_Det# = P.error "No implementation of profiledotgarbageCollectorOn_Det#"
profiledotgarbageCollectorOn_ND# = P.error "No implementation of profiledotgarbageCollectorOn_ND#"

profiledotgarbageCollect_Det# = fromForeign S.performGC
profiledotgarbageCollect_ND# = P.return P.$ P.fmap from profiledotgarbageCollect_Det#