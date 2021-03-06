{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlON.WebServer (
    runWarp, runWarp'
  ) where

import Data.Maybe
import Control.Monad
import Control.Concurrent.STM
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.EventSource
import Crypto.Skein
import qualified Data.Serialize as S
import qualified Crypto.Classes as Crypto
import qualified Data.ByteString.Base16 as B16
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import qualified Network.HTTP.Types as HTTP
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Control.Concurrent
import Network.Wai.Middleware.Cors

import AlON.Source
import AlON.Run

data SiteStruct =
  SS { sContent :: DirTree AnyContent
       -- ^ A mapping of the current content.
     , ssEvents :: DirTree (TChan ServerEvent)
       -- ^ A stream of version updates about the resource.
     }

contentResponse :: AlONContent a => a -> WAI.Response
contentResponse a =
  WAI.responseLBS (alonContentStatus a) (alonContentHeaders a) (alonContentBody a)

runWarp :: Warp.Settings -> AlONSite -> IO ()
runWarp settings site = do
  siteState <- newTVarIO (SS mempty mempty)
  let startSite is = do
        print $ T.intercalate "/" . fst <$> LT.toList is
        cm <- mapM (const newTChanIO) is
        atomically . writeTVar siteState $ SS is cm
        -- fire off warp
        void . forkIO . Warp.runSettings settings .
          cors (const . Just $ CorsResourcePolicy Nothing ["GET"] ["Accept"] Nothing Nothing False False True) $
          \r mk -> do
            ss <- atomically . readTVar $ siteState
            let lkUp = LT.lookup (WAI.pathInfo r)
            case (,) <$> (lkUp . sContent $ ss) <*> (lkUp . ssEvents $ ss) of
              Nothing -> mk . WAI.responseBuilder HTTP.notFound404 [] . fromText $ "Not found"
              Just (_, c) | ("GET" == WAI.requestMethod r) &&
                            (fromMaybe False . fmap (elem "text/event-stream" . T.splitOn ", " . TE.decodeUtf8) . lookup HTTP.hAccept . WAI.requestHeaders $ r) -> do
                cc <- atomically . cloneTChan $ c
                eventSourceAppIO (atomically . readTChan $ cc) r mk
              Just (d, _) | ("GET" == WAI.requestMethod r) ->
                mk $ contentResponse d
              _ -> mk . WAI.responseBuilder HTTP.methodNotAllowed405 [] . fromText $ "GET only"
  let upSite ups = do
       print $ T.intercalate "/" . fst <$> ups
       atomically $ do
        i' <- readTVar siteState
        inxt <- (\a -> foldM a i' ups) $ \ i (fp, md) -> do
           case md of
             Nothing -> do
                     maybe (return ()) (flip writeTChan CloseEvent) . LT.lookup fp . ssEvents $ i
                     return $ SS (LT.delete fp . sContent $ i) (LT.delete fp . ssEvents $ i)
             Just d -> do
                     c <- maybe newTChan return . LT.lookup fp . ssEvents $ i
                     let h = Crypto.hash (alonContentBody d)::Skein_1024_1024
                     writeTChan c $
                       ServerEvent (Just . fromText $ "update")
                                   (Just . fromByteString . B16.encode . S.encode $ h)
                                   []
                     return $ SS (LT.insert' fp d . sContent $  i) (LT.insert fp c . ssEvents $ i)
        writeTVar siteState inxt
  runSite (TIO.putStrLn . T.intercalate "\n") startSite upSite site

runWarp' :: Warp.Settings -> AlONSite -> IO ()
runWarp' settings site = do
  siteState <- newTVarIO (SS mempty mempty)
  void . forkIO . Warp.runSettings settings .
          cors (const . Just $ CorsResourcePolicy Nothing ["GET"] ["Accept"] Nothing Nothing False False True) $
          \r mk -> do
            ss <- atomically . readTVar $ siteState
            let lkUp = LT.lookup (WAI.pathInfo r)
            case (,) <$> (lkUp . sContent $ ss) <*> (lkUp . ssEvents $ ss) of
              Nothing -> mk . WAI.responseBuilder HTTP.notFound404 [] . fromText $ "Not found"
              Just (_, c) | ("GET" == WAI.requestMethod r) &&
                            (fromMaybe False . fmap (elem "text/event-stream" . T.splitOn ", " . TE.decodeUtf8) . lookup HTTP.hAccept . WAI.requestHeaders $ r) -> do
                cc <- atomically . cloneTChan $ c
                eventSourceAppIO (atomically . readTChan $ cc) r mk
              Just (d, _) | ("GET" == WAI.requestMethod r) ->
                mk $ contentResponse d
              _ -> mk . WAI.responseBuilder HTTP.methodNotAllowed405 [] . fromText $ "GET only"
  let startSite is = do
        cm <- mapM (const newTChanIO) is
        atomically . writeTVar siteState $ SS is cm
        -- fire off warp
  let upSite ups = do
       atomically $ do
        i' <- readTVar siteState
        inxt <- (\a -> foldM a i' ups) $ \ i (fp, md) -> do
           case md of
             Nothing -> do
                     maybe (return ()) (flip writeTChan CloseEvent) . LT.lookup fp . ssEvents $ i
                     return $ SS (LT.delete fp . sContent $ i) (LT.delete fp . ssEvents $ i)
             Just d -> do
                     c <- maybe newTChan return . LT.lookup fp . ssEvents $ i
                     let h = Crypto.hash (alonContentBody d)::Skein_1024_1024
                     writeTChan c $
                       ServerEvent (Just . fromText $ "update")
                                   (Just . fromByteString . B16.encode . S.encode $ h)
                                   []
                     return $ SS (LT.insert' fp d . sContent $  i) (LT.insert fp c . ssEvents $ i)
        writeTVar siteState inxt
  runSite (TIO.putStrLn . T.intercalate "\n") startSite upSite site