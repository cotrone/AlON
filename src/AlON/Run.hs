{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes, FlexibleContexts, ScopedTypeVariables, GADTs #-}
{-# LANGUAGE LambdaCase #-}
module AlON.Run (
    SiteResult, AlONSitePart, AlONSite, HandleErrors, UpdateSite, SetupSite, AlONContent(..), AnyContent
  , initSite, runSite, SimpleAlONSite, hostSimpleAlON, initSimpleAlON
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.EQueue
import Control.Monad.Trans
import Control.Monad.Reader
import AlON.Types
import AlON.Source
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import Reflex
import Reflex.Host.Class
import Data.Functor.Misc
import Data.Functor.Identity
import Data.Dependent.Sum (DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import Control.Monad.Loops
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Text (Text)
import Data.List
import Data.IORef
import Control.Monad.Ref
import Control.Monad.Primitive
import AlON.ContentType.StaticFile
import Reflex.Host.Headless

type SiteResult t = DynDirTree t AnyContent

type AlONSitePart t a =
  (Reflex t, ReflexHost t, MonadIO (HostFrame t), MonadSubscribeEvent t (HostFrame t)) =>
  AlONT t (HostFrame t) a

type AlONSite =
  forall t. (Reflex t, ReflexHost t, MonadIO (HostFrame t), MonadSubscribeEvent t (HostFrame t)) =>
  AlONSitePart t (SiteResult t)

type UpdateSite = [([Text], Maybe AnyContent)] -> IO ()

type SetupSite = DirTree AnyContent -> IO ()

type HandleErrors = [Text] -> IO ()

type SimpleAlONSite t m = MonadHeadlessApp t m => m (DynDirTree t AnyContent)

hostSimpleAlON :: (forall t m. SimpleAlONSite t m) -> SetupSite -> IO ()
hostSimpleAlON site setup =
  runHeadlessApp $ do
    siteBehavior <- flattenDynDirTree <$> site
    postBuild <- getPostBuild
    performEvent_ $ liftIO . setup <$> pushAlways (const $ sample $ current siteBehavior) postBuild
    performEvent_ $ liftIO . setup <$> updated siteBehavior
    pure never

initSimpleAlON :: (forall t m. SimpleAlONSite t m) -> SetupSite -> IO ()
initSimpleAlON site setup =
  runHeadlessApp $ do
    siteBehavior <- flattenDynDirTree <$> site
    postBuild <- getPostBuild
    setupEvent <- performEvent $ liftIO . setup <$> pushAlways (const $ sample $ current siteBehavior) postBuild
    -- Quit after the setup event has been performed
    pure setupEvent

getConName :: Typeable a => a -> Text
getConName = T.pack . tyConName . typeRepTyCon . typeOf


-- | This is just the initialization step of runSite
-- Just copied over and not reused because the error dynamic from initialization
-- need to be sampled for updates
initSite :: HandleErrors -> SetupSite -> AlONSite -> IO ()
initSite herr setup frm = runSpiderHost $ do
  startTime' <- liftIO getCurrentTime
  eq <- newSTMEQueue
  (o, errD) <- runHostFrame . runDynamicWriterT . (`runReaderT` eq) . unAlON $ frm

  pre <- waitEQ eq ReturnImmediate
  fireEvents pre
  fstate <- (sample . current $ o) >>= mapM (sample . current)
  errs' <- sample . current $ errD
  liftIO . herr $ errs'
  liftIO . setup $ fstate
  finishedTime' <- liftIO getCurrentTime
  liftIO . putStrLn $ ("Setup ("++(show $ finishedTime' `diffUTCTime` startTime')++")")

runSite :: HandleErrors -> SetupSite -> UpdateSite -> AlONSite -> IO ()
runSite herr setup up frm = runSpiderHost $ do
  startTime' <- liftIO getCurrentTime

  eq <- newSTMEQueue
  (o, errD) <- runHostFrame . runDynamicWriterT . (`runReaderT` eq) . unAlON $ frm

  pre <- waitEQ eq ReturnImmediate
  fireEvents pre
  fstate <- (sample . current $ o) >>= mapM (sample . current)
  errs' <- sample . current $ errD
  liftIO . herr $ errs'
  liftIO . setup $ fstate
  finishedTime' <- liftIO getCurrentTime
  liftIO . putStrLn $ ("Setup ("++(show $ finishedTime' `diffUTCTime` startTime')++")")

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
  let existingPages'::DMap.DMap (Const2 [Text] AnyContent) (Event Spider) = DMap.fromList . map (\(k, v) -> (Const2 k) :=> (updated $ v)) . LT.toList $ initialMapping
  (`iterateM_` (initialMapping, existingPages')) $ \(lastMapping, formerExistingPages) -> do

    -- Read the new events
    es <- waitEQ eq RequireEvent

    startTime <- liftIO getCurrentTime

    pageChangeHandle <- subscribeEvent . merge $ formerExistingPages
    ec::[([Text], Maybe AnyContent)] <- fireEventsAndRead es $ do
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

    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Added"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) added)
    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Removed"::T.Text]:(fmap (\(t, _) -> (" - ":t)) removed)
    liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Changed"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) ec)

    return (newMapping, newPages)
