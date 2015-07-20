{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ALON.Source (
    time
  , DirTree, DataUpdate(..)
  , dirSource
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Filesystem as FS
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.FSNotify as FSN
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators (sourceDirectoryDeep)
import qualified Control.Exception as E
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time

time :: (MonadIO m, MonadHold Spider m, MonadReflexCreateTrigger Spider m)
     => TQueue (DSum (EventTrigger Spider))
     -> DiffTime -> m (Dynamic Spider UTCTime)
time q dt = do
  e <- newEventWithTrigger $ \et -> do
    t <- forkIO . forever $ do
           getCurrentTime >>= (atomically . writeTQueue q . (et :=>))
           -- This drifts. Not considered a problem for its intended use.
           threadDelay . floor $ dt*(10^(6::Int))
    return $ killThread t
  now <- liftIO getCurrentTime
  holdDyn now e

type DirTree a = TrieMap FP.FilePath a

data DataUpdate =
    DataMod ByteString
  | DataDel
  deriving (Eq, Ord, Show)

dirSource :: (MonadIO m, MonadHold Spider m, MonadReflexCreateTrigger Spider m, MonadFix m)
          => TQueue (DSum (EventTrigger Spider))
          -> FP.FilePath -> m (Dynamic Spider (DirTree (Dynamic Spider ByteString)))
dirSource eq dir = do
    de <- newEventWithTrigger $ \et -> do
      t <- forkIO . FSN.withManagerConf (FSN.defaultConfig {FSN.confUsePolling = False}) $ \m -> do
        E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
          wq <- newTQueueIO
          -- Start listenin before we read the dir
          void . FSN.watchTree m dir (const True) $ atomically . writeTQueue wq
          -- Then we just watch the changes, and send them on
          forever . E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
            e <- atomically (readTQueue wq) >>= e2e
            atomically . writeTQueue eq $ et :=> e
      return (killThread t)
    let doDyn fl di = foldDyn (\e v ->
                                  case e of
                                    (fp, DataMod d) | fp == fl -> d
                                    _ -> v) di de
        doDirTree c t =
            case c of
               (fp, DataDel) -> return . LT.delete fp $ t
               (fp, DataMod _) | LT.member fp t -> return t -- It'll update its self.
               (fp, DataMod d) | otherwise -> (\v -> LT.insert fp v t) <$> doDyn fp d
    initS <- liftIO readDb
    initDir <- foldM (flip doDirTree) mempty initS
    foldDynM doDirTree initDir de
  where
    e2e :: FSN.Event -> IO ([FP.FilePath], DataUpdate)
    e2e (FSN.Added fp _) = r fp
    e2e (FSN.Modified fp _) = r fp
    e2e (FSN.Removed fp _) = return $ (FP.splitDirectories fp, DataDel)
    r :: FP.FilePath -> IO ([FP.FilePath], DataUpdate)
    r fp = do
      d <- liftIO . FS.readFile $ fp
      d `deepseq` return (FP.splitDirectories fp, DataMod d)
    readDb :: IO [([FP.FilePath], DataUpdate)]
    readDb = do
      runResourceT $
        sourceDirectoryDeep True (FP.encodeString dir) $= CL.mapM (liftIO . r . FP.decodeString) $$ CL.consume
