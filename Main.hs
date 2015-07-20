{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import ALON.Source
import Reflex
import Reflex.Host.Class
import Control.Concurrent.STM
import Data.Dependent.Sum (DSum)
import Data.ByteString (ByteString)
import Control.Monad.Loops
import qualified Filesystem.Path as FP
import qualified Data.ListTrie.Patricia.Map.Ord as LT

type SiteResult = Dynamic Spider (DirTree (Dynamic Spider ByteString))

type AlONSite = TQueue (DSum (EventTrigger Spider)) -> HostFrame Spider SiteResult

type UpdateSite = [([FP.FilePath], ByteString)] -> IO ()

type SetupSite = DirTree ByteString -> IO ()

runSite :: SetupSite -> UpdateSite -> AlONSite -> IO ()
runSite setup up frm = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  o <- runHostFrame . frm $ eq

  pre <- liftIO . atomically . whileM (not <$> isEmptyTQueue eq) $ readTQueue eq
  fireEvents pre
  fstate <- (sample . current $ o) >>= mapM (sample . current)
  liftIO . setup $ fstate

  void . forever $ do
    e <- liftIO . atomically $ readTQueue eq
    outer <- sample . current $ o
    eh <- mapM (subscribeEvent . updated) $ outer
    oc <- fireEventsAndRead [e] $ do
                     oces <- LT.mapMaybe id <$> mapM readEvent eh
                     mapM id oces
    liftIO . putStrLn $ "Events"
    liftIO . up . LT.toList $ oc

main :: IO ()
main =
    runSite print (mapM_ print) frm
  where
    frm eq = do
      --et <- time eq 1
      dt <- dirSource eq "test_dir"
      return dt
