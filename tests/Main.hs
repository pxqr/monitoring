{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import System.Monitoring
import System.Monitoring.GC
import Data.HashMap.Strict as HM
import Data.Aeson

work :: Monitor -> Int -> Int -> IO ()
work ch i n = do
  signalEvent ch $ Update "main" $ HM.fromList
    [ ("iteration", toJSON n)
    , ("interval" , toJSON i)
    ]
  threadDelay i
  work ch i (succ n)

main :: IO ()
main = do
  ch <- monitoring 3000
  rtsStats ch
  work ch 100000 0