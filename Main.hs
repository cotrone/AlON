{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import ALON.Source
import Reflex
import Reflex.Host.Class
import Reflex.Spider
import Reflex.Spider.Internal (ResultM)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

timePulse :: MonadReflexCreateTrigger Spider m => TQueue (DSum (EventTrigger Spider)) -> DiffTime -> m (Event Spider UTCTime)
timePulse q dt = 

main :: IO ()
main = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  de <- dirSource eq "test_dir"

  o <- runHostFrame $ do
    
    return de

  forever $ do
    e <- liftIO . atomically $ readTQueue eq
    r <- fireEventsAndRead [e] (return ())
    liftIO $ print r

  return ()
