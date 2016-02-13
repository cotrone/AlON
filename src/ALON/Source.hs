{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module ALON.Source (
    time
  , DirTree, DataUpdate(..)
  , dirSource
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified System.FilePath as FP
import qualified System.FSNotify as FSN
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=), ($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators (sourceDirectoryDeep)
import qualified Control.Exception as E
import Data.Functor.Misc
import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity

import ALON.Types

time :: (Reflex t, MonadALON t m)
     => DiffTime -> m (Dynamic t UTCTime)
time dt = do
  eq <- askEQ
  e <- newEventWithTrigger $ \et -> do
    t <- forkIO . forever $ do
           getCurrentTime >>= (atomically . writeTQueue eq . (et :=>) . Identity)
           -- This drifts. Not considered a problem for its intended use.
           threadDelay . floor $ dt*(10^(6::Int))
    return $ killThread t
  now <- liftIO getCurrentTime
  holdDyn now e

type DirTree a = TrieMap Text a

data DataUpdate =
    DataMod ByteString
  | DataDel
  deriving (Eq, Ord, Show)

dirSource :: (Reflex t, MonadALON t m)
          => FP.FilePath -> m (Dynamic t (DirTree (Dynamic t ByteString)))
dirSource dir = do
    eq <- askEQ
    de <- newEventWithTrigger $ \et -> do
      t <- forkIO . FSN.withManagerConf (FSN.defaultConfig {FSN.confUsePolling = False}) $ \m -> do
        E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
          wq <- newTQueueIO
          -- Start listenin before we read the dir
          void . FSN.watchTree m dir (const True) $ atomically . writeTQueue wq
          -- Then we just watch the changes, and send them on
          -- Having stripped off the leading path
          pwd <- getCurrentDirectory
          forever . E.handle (\(e::E.SomeException) -> putStrLn ("Excp: "++show e)) $ do
            e <- atomically (readTQueue wq) >>= e2e pwd
            atomically . writeTQueue eq $ et :=> (Identity e)
      return (killThread t)
    let des = fanMap $ (uncurry Map.singleton) <$> de
        flEvent = select des . Const2 
        --flEvent fl = fmap snd . ffilter ((==) fl . fst) $ de
        doDyn fl di = foldDyn (\e v ->
                                  case e of
                                    DataMod d -> d
                                    _ -> v) di . flEvent $ fl
        doDirTree c t =
            case c of
               (fp, DataDel) -> return . LT.delete fp $ t
               (fp, DataMod _) | LT.member fp t -> return t -- It'll update its self.
               (fp, DataMod d) | otherwise -> (\v -> LT.insert fp v t) <$> doDyn fp d
    -- Place de has to be active by.
    initS <- liftIO readDb
    initDir <- foldM (flip doDirTree) mempty initS
    foldDynM doDirTree initDir de
  where
    e2e :: FP.FilePath -> FSN.Event -> IO ([Text], DataUpdate)
    e2e pf (FSN.Added fp _) = r pf fp
    e2e pf (FSN.Modified fp _) = r pf fp
    e2e pf (FSN.Removed fp _) =
        return $ ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                 , DataDel)
    r :: FP.FilePath -> FP.FilePath -> IO ([Text], DataUpdate)
    r pf fp = do
      d <- liftIO . BS.readFile $ fp
      d `deepseq` return ( map T.pack . drop 1 . FP.splitDirectories . FP.makeRelative pf $ fp
                         , DataMod d)
    readDb :: IO [([Text], DataUpdate)]
    readDb = do
      runResourceT $
        sourceDirectoryDeep True dir $= CL.mapM (liftIO . r "") $$ CL.consume
