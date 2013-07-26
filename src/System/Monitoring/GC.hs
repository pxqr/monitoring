{-# LANGUAGE TemplateHaskell #-}
module System.Monitoring.GC
       ( updateStats
       , rtsStats
       ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import GHC.Stats
import Foreign.Ptr
import Foreign.Concurrent
import System.Mem.Weak

import System.Monitoring

onNextGC :: IO () -> IO ()
onNextGC = void . newForeignPtr nullPtr

onGC :: IO () -> IO ()
onGC action = onNextGC $ action >> onGC action

$(deriveJSON id ''GCStats)

signalStats :: Monitor -> GCStats -> IO ()
signalStats es stats = signalEvent es $ Update "GC" x
  where
    Object x = toJSON stats
  {-
  signalEvent es $ Update "numGCs"                 $ fromIntegral numGcs

  signalEvent es $ Update "bytesAllocated"         $ fromIntegral bytesAllocated
  signalEvent es $ Update "peakMegabytesAllocated" $ fromIntegral peakMegabytesAllocated

  signalEvent es $ Update "currentBytesSlop"       $ fromIntegral currentBytesSlop
  signalEvent es $ Update "maxBytesSlop"           $ fromIntegral maxBytesSlop

  signalEvent es $ Update "currentBytesUsed"       $ fromIntegral currentBytesUsed
  signalEvent es $ Update "cumulativeBytesUsed"    $ fromIntegral cumulativeBytesUsed
  signalEvent es $ Update "maxBytesUsed"           $ fromIntegral maxBytesUsed

  signalEvent es $ Update "bytesCopied"            $ fromIntegral bytesCopied
  signalEvent es $ Update "parTotBytesCopied"      $ fromIntegral parTotBytesCopied
  signalEvent es $ Update "parMaxBytesCopied"      $ fromIntegral parMaxBytesCopied

  signalEvent es $ Update "mutatorCpuSeconds"      $ floor mutatorCpuSeconds
  signalEvent es $ Update "mutatorWallSeconds"     $ floor mutatorWallSeconds
  signalEvent es $ Update "gcCpuSeconds"           $ floor gcCpuSeconds
  signalEvent es $ Update "gcWallSeconds"          $ floor gcWallSeconds
  signalEvent es $ Update "cpuSeconds"             $ floor cpuSeconds
  signalEvent es $ Update "wallSeconds"            $ floor wallSeconds
-}

updateStats :: Monitor -> IO ()
updateStats es = getGCStats >>= signalStats es

rtsStats :: Monitor -> IO ()
rtsStats es = do
  updateStats es
  onGC (updateStats es)
