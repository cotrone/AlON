{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import ALON.Source
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Reflex.Spider
import Reflex.Spider.Internal (ResultM)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Maybe

main :: IO ()
main = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  o <- runHostFrame $ do
    et <- time eq 0.1
    dt <- dirSource eq "test_dir" >>= hold []
    return et

  forever $ do
    e <- liftIO . atomically $ readTQueue eq
    eh <- subscribeEvent . updated $ o
    oc <- fireEventsAndRead [e] (isJust <$> (readEvent eh))
    r <- sample (current o)
    liftIO $ print oc
    liftIO $ print r

  return ()
