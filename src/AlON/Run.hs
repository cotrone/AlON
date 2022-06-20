{-# LANGUAGE KindSignatures, OverloadedStrings, RankNTypes, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE LambdaCase, ConstraintKinds, TupleSections #-}
module AlON.Run (
    AlONSite, AlONSiteResult(..), HandleErrors, UpdateSite, SetupSite, AlONContent(..), AnyContent
  , initSite, runSite
  ) where
import AlON.Types
import AlON.Source
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.EQueue
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Reader
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
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , ReflexHost t
  , Reflex t
  , TriggerEvent t m
  )

type AlONSite = forall t m. MonadAlON' t m => m (AlONSiteResult t)

data AlONSiteResult t = AlONSiteResult {
  alonSiteContent :: DynDirTree t AnyContent
, alonSiteReady :: Dynamic t Bool
}

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
    fstate <- (sample . current $ (alonSiteContent siteRes)) >>= mapM (sample . current)
    errs' <- sample . current $ errD
    liftIO . herr $ errs'
    liftIO . setup $ fstate
    finishedTime' <- liftIO getCurrentTime
    liftIO . putStrLn $ ("Setup ("++(show $ finishedTime' `diffUTCTime` startTime')++")")

data RunSiteStep t =
    EQueueTriggers [DSum (EventTrigger t) Identity]
  | PerformEventTriggers [DSum (EventTriggerRef t) TriggerInvocation]
  | EQueueThreadExited (Either SomeException ())
  | PerformEventThreadExited (Either SomeException ())

exitReason :: Either SomeException () -> String
exitReason (Left ex) = "Exception: " <> show ex
exitReason (Right ()) = "Graceful"

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
              flip runReaderT eq $
                runDynamicWriterT frm 

    _ <- maybe (pure ()) (\t -> void . fire [t :=> Identity ()] $ pure ()) =<< readRef postBuildTriggerRef

    pre <- waitEQ eq ReturnImmediate
    fireEvents pre
    fstate <- (sample . current $ alonSiteContent siteRes) >>= mapM (sample . current)
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
    initialMapping <- sample . current $ alonSiteContent siteRes
    let existingPages'::DMap.DMap (Const2 [Text] AnyContent) (Event Spider) = DMap.fromList . map (\(k, v) -> (Const2 k) :=> (updated $ v)) . LT.toList $ initialMapping
    
    nextEvent <- liftIO $ atomically $ newEmptyTMVar

    equeueThread <- liftIO $ async $ forever $ do
      es <- waitEQ eq RequireEvent
      atomically $ putTMVar nextEvent $ EQueueTriggers es

    performEventThread <- liftIO $ async $ forever $ do
      ev <- readChan events
      atomically $ putTMVar nextEvent $ PerformEventTriggers ev
    let readStep = atomically $ 
              EQueueThreadExited <$> waitCatchSTM equeueThread
          <|> PerformEventThreadExited <$> waitCatchSTM performEventThread
          <|> takeTMVar nextEvent
              

    (`iterateM_` (initialMapping, existingPages')) $ \(lastMapping, formerExistingPages) -> do
      _ <- liftIO $ mapM print $ listKeys formerExistingPages
      pageChangeHandle <- subscribeEvent . merge $ formerExistingPages
      next <- liftIO readStep
      startTime <- liftIO getCurrentTime
      ec :: [([Text], Maybe AnyContent)] <- case next of
        EQueueTriggers es -> do
          liftIO $ putStrLn "Running equeue triggers"
          fmap concat $ fire es $ do
            mchange <- readEvent pageChangeHandle
            changes <- maybe (return mempty) id mchange
            return .  map (\((Const2 k) :=> v) -> (k, Just . runIdentity $ v)) . DMap.toList $ changes
        PerformEventTriggers ev -> do
          liftIO $ putStrLn "Running PerformEvent triggers"
          fmap concat $ fireEventTriggerRefs fc ev $ do
            mchange <- readEvent pageChangeHandle
            changes <- maybe (return mempty) id mchange
            return .  map (\((Const2 k) :=> v) -> (k, Just . runIdentity $ v)) . DMap.toList $ changes
        EQueueThreadExited exit -> do
          liftIO $ putStrLn $ "Equeue thread exited: " <> exitReason exit
          pure []
        PerformEventThreadExited exit -> do
          liftIO $ putStrLn $ "PerformEvent thread exited: " <> exitReason exit
          pure []

      liftIO $ print ec
      newMapping <- sample . current $ alonSiteContent siteRes

      let addedDyn = LT.toList . LT.difference newMapping $ lastMapping
      added <- (fmap (fmap Just)) <$> (mapM (mapM (sample . current)) addedDyn) 
      let removed = fmap (fmap $ const Nothing) . LT.toList . LT.difference lastMapping $ newMapping
      modifiedDyn <- modifiedDynDirTree lastMapping newMapping
      modified <-  (fmap (fmap Just)) <$> (mapM (mapM (sample . current)) modifiedDyn) 
      -- update the existing page watch so we know about changes to internal pages.
      let withAdded = foldl (\m (k, v) -> DMap.insert (Const2 k) (updated v) m) formerExistingPages $ addedDyn
      let withModified = foldl (\m (k, v) -> DMap.insert (Const2 k) (updated $ v) m) withAdded $ modifiedDyn
      let newPages  =  foldl (\m (k, _) -> DMap.delete (Const2 k) m) withModified $ removed
      
      errs <- sample . current $ errD

      finishedTime <- liftIO getCurrentTime
      liftIO . putStrLn $ ("Events ("++(show $ finishedTime `diffUTCTime` startTime)++")")

      -- Take the resulting actions, logging the errors and updating the site.
      liftIO . herr $ errs
      liftIO . up $ ec++added++removed++modified

      liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Added"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) added)
      liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Removed"::T.Text]:(fmap (\(t, _) -> (" - ":t)) removed)
      liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Changed"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) ec)
      -- liftIO . TIO.putStr . mconcat . map (flip T.append "\n" . mconcat . intersperse "/") $ ["Modified"::T.Text]:(fmap (\(t, v) -> (" - ":t) `mappend` [" : " `T.append` (getConName v)]) modified)

      return (newMapping, newPages)
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


listKeys :: DMap.DMap (Const2 [Text] AnyContent) (Event Spider) -> [[Text]]
listKeys = fmap f . DMap.toList
  where
    f :: DSum (Const2 [Text] AnyContent) (Event Spider) -> [Text]
    f (Const2 ts :=> _) = ts

modifiedDynDirTree :: forall t m. (Reflex t, MonadSample t m)
                   => DirTree (Dynamic t AnyContent)
                   -> DirTree (Dynamic t AnyContent)
                   -> m [([T.Text], Dynamic t AnyContent)]
modifiedDynDirTree old new =
  fmap catMaybes $ mapM (\(t, r) -> fmap (t,) <$> r) $ LT.toList $ LT.intersectionWith f old new
  where
    f o' n' = do
      o <- sample $ current o'
      n <- sample $ current n'
      if contentPieces o == contentPieces n
        then pure Nothing
        else pure $ Just n'
    contentPieces c = (alonContentStatus c, alonContentHeaders c, alonContentBody c)
