{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import ALON.Source
import Reflex
import Reflex.Host.Class
import Control.Concurrent.STM

import Data.Maybe

main :: IO ()
main = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  o <- runHostFrame $ do
    --et <- time eq 1
    dt <- dirSource eq "test_dir"
    return dt

  void . forever $ do
    e <- liftIO . atomically $ readTQueue eq
    eh <- subscribeEvent . updated $ o
    oc <- fireEventsAndRead [e] (isJust <$> (readEvent eh))
    r <- sample (current o)
    liftIO . print $ oc
    liftIO . print . fmap (const ()) $ r

  return ()
