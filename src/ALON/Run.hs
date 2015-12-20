{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module ALON.Run (
    SiteResult, AlONSite, UpdateSite, SetupSite
  , runSite
  ) where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import ALON.Types
import ALON.Source
import Data.Time
import Reflex
import Reflex.Host.Class
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Control.Monad.Loops
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Text (Text)

type SiteResult t = Dynamic t (DirTree (Dynamic t ByteString))

type AlONSite =
  forall t. (Reflex t, ReflexHost t, Monad (HostFrame t), MonadIO (HostFrame t)) =>
  ALONT t (HostFrame t) (SiteResult t)

type UpdateSite = [([Text], Maybe ByteString)] -> IO ()

type SetupSite = DirTree ByteString -> IO ()

runSite :: SetupSite -> UpdateSite -> AlONSite -> IO ()
runSite setup up frm = runSpiderHost $ do
  eq <- liftIO newTQueueIO

  o <- runHostFrame . (`evalStateT` (constDyn [])) . (`runReaderT` eq) . unALON $ frm

  pre <- liftIO . atomically . whileM (not <$> isEmptyTQueue eq) $ readTQueue eq
  fireEvents pre
  fstate <- (sample . current $ o) >>= mapM (sample . current)
  liftIO . setup $ fstate

  {- Ok, this gets a bit complicated.
   -
   - So, we can't look inside the tree when we read events.
   - So we read the state of the tree, and save that.
   - Use it to generate a list of things to also listen to for events.
   - Listen for changes in the tree, which captures any *pre existing*
   - events that change their value.
   - But, it *misses* events of elements that are deleted, or elements that are added.
   - So after the update we read the tree *again* and do tree subtraction in *both* directions.
   - Anything left from removing the old tree from the new is a new entry.
   - Anything left from removing the new tree from the old is a deleted entry.
   - And, since they can't be in the events we got from the update, we know there is no overlap.
   -}
  tss' <- sample . current $ o
  (`iterateM_` tss') $ \tss -> do
    e <- liftIO . atomically $ readTQueue eq
    startTime <- liftIO getCurrentTime
    ecw <- mapM (subscribeEvent . updated) $ tss
    ec <- fireEventsAndRead [e] $ do
                     oces <- mapM readEvent $ ecw
                     (LT.toList . fmap Just) <$> (sequenceA . LT.mapMaybe id $ oces)
    tes <- sample . current $ o
    added <- (fmap (fmap Just) . LT.toList) <$> (mapM (sample . current) . LT.difference tes $ tss)
    let remed = fmap (fmap $ const Nothing) . LT.toList . LT.difference tss $ tes
    finishedTime <- liftIO getCurrentTime
    liftIO . putStrLn $ ("Events ("++(show $ finishedTime `diffUTCTime` startTime)++")")
    liftIO . up $ ec++added++remed
    return tes
