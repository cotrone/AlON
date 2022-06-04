{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE LambdaCase, ConstraintKinds #-}
module AlON.Run (
    AlONSite, HandleErrors, UpdateSite, SetupSite, AlONContent(..), AnyContent
  , initSite, runSite
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
import Data.Dependent.Sum (DSum((:=>)), (==>))
import qualified Data.Dependent.Map as DMap
import Control.Monad.Loops
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Text (Text)
import Data.List
import Control.Monad.Ref
import Safe (lastMay)

import Data.Foldable (for_)
import Data.IORef (readIORef)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

type UpdateSite = [([Text], Maybe AnyContent)] -> IO ()

type SetupSite = DirTree AnyContent -> IO ()

type HandleErrors = [Text] -> IO ()

type EQueueT t = STMEQueue (DSum (EventTrigger t) Identity)

type MonadAlON' t m = 
  ( Adjustable t m
  , DynamicWriter t [Text] m
  , MonadFix m
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadReader (EQueueT t) m
  , MonadRef (HostFrame t)
  , MonadSubscribeEvent t (HostFrame t)
  , PerformEvent t m
  , PostBuild t m
  , ReflexHost t
  , Reflex t
  , TriggerEvent t m
  )

type AlONSite = forall t m. MonadAlON' t m => m (DynDirTree t AnyContent)

getConName :: Typeable a => a -> Text
getConName = T.pack . tyConName . typeRepTyCon . typeOf

initSite :: HandleErrors -> SetupSite -> AlONSite -> IO ()
initSite herr setup frm =
  runSpiderHost $ do
    startTime' <- liftIO getCurrentTime
    events <- liftIO newChan
    eq <- newSTMEQueue
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    ((siteRes, errD), FireCommand fire) <-
      hostPerformEventT $
          flip runPostBuildT postBuild $
            flip runTriggerEventT events $
              flip runReaderT eq $
                runDynamicWriterT frm 

    pre <- waitEQ eq ReturnImmediate
    _ <- maybe (pure ()) (\t -> void . fire [t :=> Identity ()] $ pure ()) =<< readRef postBuildTriggerRef
    _ <- fire pre $ pure ()
    fstate <- (sample . current $ siteRes) >>= mapM (sample . current)
    errs' <- sample . current $ errD
    liftIO . herr $ errs'
    liftIO . setup $ fstate
    finishedTime' <- liftIO getCurrentTime
    liftIO . putStrLn $ ("Setup ("++(show $ finishedTime' `diffUTCTime` startTime')++")")


-- | This is just the initialization step of runSite
-- Just copied over and not reused because the error dynamic from initialization
-- need to be sampled for updates
runSite :: HandleErrors -> SetupSite -> UpdateSite -> AlONSite -> IO ()
runSite herr setup up frm = 
 runSpiderHost $ do
  startTime' <- liftIO getCurrentTime
  events <- liftIO newChan
  eq <- newSTMEQueue
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

  ((siteRes, errD), fc@(FireCommand fire)) <-
    hostPerformEventT $
        flip runPostBuildT postBuild $
          flip runTriggerEventT events $
            flip runReaderT eq
            $ runDynamicWriterT frm 

  _ <- maybe (pure ()) (\t -> void . fire [t :=> Identity ()] $ pure ()) =<< readRef postBuildTriggerRef
  pre <- waitEQ eq ReturnImmediate
  _ <- fire pre $ pure ()
  fstate <- (sample . current $ siteRes) >>= mapM (sample . current)
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
  -- initialMapping <- sample . current $ siteRes
  -- let existingPages'::DMap.DMap (Const2 [Text] AnyContent) (Event Spider) = DMap.fromList . map (\(k, v) -> (Const2 k) :=> (updated $ v)) . LT.toList $ initialMapping
  -- (`iterateM_` (initialMapping, existingPages')) $ \(lastMapping, formerExistingPages) -> do

    -- Read the new events
    -- es <- waitEQ eq RequireEvent
  forever $ do

    ers <- liftIO $ readChan events
    startTime <- liftIO getCurrentTime
    newSiteUpdate <- subscribeEvent $ updated siteRes
    ec :: [Maybe (DirTree (Dynamic t AnyContent))] <- fireEventTriggerRefs fc ers $ sequence =<< readEvent newSiteUpdate
    dynRes <- mapM (mapM (sample . current)) $ catMaybes ec

    -- let addedDyn   = LT.toList . LT.difference newMapping $ lastMapping
    -- added <- (fmap (fmap Just)) <$> (mapM (mapM (sample . current)) addedDyn) 
    -- let removed = fmap (fmap $ const Nothing) . LT.toList . LT.difference lastMapping $ newMapping

    -- -- update the existing page watch so we know about changes to internal pages.
    -- let withAdded = foldl (\m (k, v) -> DMap.insert (Const2 k) (updated $ v) m) formerExistingPages $ addedDyn
    -- let newPages  =  foldl (\m (k, _) -> DMap.delete (Const2 k) m) withAdded $ removed
    
    errs <- sample . current $ errD

    finishedTime <- liftIO getCurrentTime
    liftIO . putStrLn $ ("Events ("++(show $ finishedTime `diffUTCTime` startTime)++")")

    -- Take the resulting actions, logging the errors and updating the site.
    liftIO . herr $ errs
    maybe (pure ()) (liftIO . setup) $ lastMay dynRes

    -- liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Added"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) added)
    -- liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Removed"::T.Text]:(fmap (\(t, _) -> (" - ":t)) removed)
    -- liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Changed"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) ec)

    pure ()
  where
    fireEventTriggerRefs
      :: MonadIO m
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          pure $ fmap (==> a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      pure a
