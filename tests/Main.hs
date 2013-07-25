{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import System.Monitoring
import System.Monitoring.GC

work ch i n = do
  signalEvent ch $ Update "iteration" n
  threadDelay i
  work ch i (succ n)

main :: IO ()
main = do
  ch <- monitoring 3000
  rtsStats ch
  work ch 10000 0