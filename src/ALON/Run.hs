{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, ScopedTypeVariables, GADTs #-}
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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Reflex
import Reflex.Host.Class
import Data.Functor.Misc
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Control.Monad.Loops
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Text (Text)
import Data.List

import qualified Debug.Trace as Debug

type SiteResult t = Dynamic t (DirTree (Dynamic t ByteString))

type AlONSite =
  forall t. (Reflex t, ReflexHost t, Monad (HostFrame t), MonadIO (HostFrame t), MonadIO (PushM t), MonadIO (PullM t)) =>
  ALONT t (HostFrame t) (SiteResult t)

type UpdateSite = [([Text], Maybe ByteString)] -> IO ()

type SetupSite = DirTree ByteString -> IO ()

type HandleErrors = [Text] -> IO ()

runSite :: HandleErrors -> SetupSite -> UpdateSite -> AlONSite -> IO ()
runSite herr setup up frm = runSpiderHost $ do
  startTime' <- liftIO getCurrentTime

  eq <- liftIO newTQueueIO
  (o, errD) <- runHostFrame . (`runStateT` (constDyn [])) . (`runReaderT` eq) . unALON $ frm

  pre <- liftIO . atomically . whileM (not <$> isEmptyTQueue eq) $ readTQueue eq
  fireEvents pre
  fstate <- (sample . current $ o) >>= mapM (sample . current)
  liftIO . setup $ fstate
  errs' <- sample . current $ errD
  finishedTime' <- liftIO getCurrentTime
  liftIO . putStrLn $ ("Setup ("++(show $ finishedTime' `diffUTCTime` startTime')++")")
  liftIO . herr $ errs'

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
  initialMapping <- sample . current $ o
  let existingPages'::DMap.DMap (Const2 [Text] ByteString) (Event Spider) = DMap.fromList . map (\(k, v) -> (Const2 k) :=> (updated $ v)) . LT.toList $ initialMapping
  (`iterateM_` (initialMapping, existingPages')) $ \(lastMapping, formerExistingPages) -> do

    -- Read the new events
    e <- liftIO . atomically $ readTQueue eq

    startTime <- liftIO getCurrentTime

    pageChangeHandle <- subscribeEvent . merge $ formerExistingPages
    ec::[([Text], Maybe ByteString)] <- fireEventsAndRead [e] $ do
      mchange <- readEvent pageChangeHandle
      changes <- maybe (return mempty) id mchange
      return .  map (\((Const2 k) :=> v) -> (k, Just . runIdentity $ v)) . DMap.toList $ changes

    newMapping <- sample . current $ o

    let addedDyn   = LT.toList . LT.difference newMapping $ lastMapping
    added <- (fmap (fmap Just)) <$> (mapM (mapM (sample . current)) addedDyn) 
    let removed = fmap (fmap $ const Nothing) . LT.toList . LT.difference lastMapping $ newMapping

    -- update the existing page watch so we know about changes to internal pages.
    let withAdded = foldl (\m (k, v) -> DMap.insert (Const2 k) (updated $ v) m) formerExistingPages $ addedDyn
    let newPages  =  foldl (\m (k, _) -> DMap.delete (Const2 k) m) withAdded $ removed
    
    errs <- sample . current $ errD

    finishedTime <- liftIO getCurrentTime
    liftIO . putStrLn $ ("Events ("++(show $ finishedTime `diffUTCTime` startTime)++")")

    -- Take the resulting actions, logging the errors and updating the site.
    liftIO . herr $ errs
    liftIO . up $ ec++added++removed

    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Added"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [T.pack . show $ v]) added)
    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Removed"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [T.pack . show $ (v::Maybe ByteString)]) removed)
    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Changed"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [T.pack . show $ v]) ec)

    return (newMapping, newPages)
