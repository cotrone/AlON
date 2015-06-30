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
import Data.Time
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Data.Maybe
    
timePulse :: (MonadIO m, MonadReflexCreateTrigger Spider m) => TQueue (DSum (EventTrigger Spider)) -> DiffTime -> m (Event Spider UTCTime)
timePulse q dt = do
  liftIO . putStrLn $ "Time event source asked for."
  newEventWithTrigger $ \et -> do
    putStrLn "Time event source started!"
    t <- forkIO . forever $ do
           getCurrentTime >>= (atomically . writeTQueue q . (et :=>))
           threadDelay . floor $ dt*(10^6)
    return (putStrLn "Time event source killed!" >> killThread t)

main :: IO ()
main = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  --de <- dirSource eq "test_dir"

  o <- runHostFrame $ do
    et <- timePulse eq 1
    now <- liftIO getCurrentTime
    hold now et

  forever $ do
    e <- liftIO . atomically $ readTQueue eq
    fireEventsAndRead [e] (return ())
    r <- sample o
    liftIO $ print r

  return ()
