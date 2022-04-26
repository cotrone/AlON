{-# LANGUAGE RankNTypes, FlexibleContexts, BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings, TupleSections #-}
module AlON.Static where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Control.Monad.RWS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Machine
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Tuple as Tuple
import Data.Void
import qualified Network.HTTP.Types as HTTP
import System.IO

import AlON.Run
import AlON

-- | Build a static site bundle, streaming the result as
-- TAR file chunks
staticizeSite :: HandleErrors
              -> AlONSite
              -> ProcessT IO LBS.ByteString Void
              -> IO ()
staticizeSite handleErrors site tarSink =
  initSite handleErrors startSite site
  where
    startSite siteContent =
      runT_ $ entrySource siteContent ~> writeTarEntries ~> tarSink
    entrySource siteContent =
      alonStaticContent handleErrors siteContent
      <> nginxConfig handleErrors (LT.mapMaybe toNginxLocationEntry siteContent)

-- | Write a tar archive from a stream of entries
writeTarEntries :: Monad m => ProcessT m Tar.Entry LBS.ByteString
writeTarEntries =
  -- Write every entry to a ByteString
  -- when that stops, write the archive terminating blocks
  tarWriter <> finalizeTar
  where
    -- The tar library doesn't export a way to write a single entry
    -- so we have to write a singleton list of entries and remove the
    -- archive terminating blocks
    tarWriter = mapping $ \entry -> LBS.dropEnd (512 * 2) $ Tar.write [entry]
    -- Write a ByteString for the archive terminating blocks
    finalizeTar = construct $ yield $ LBS.replicate (512*2) 0


-- | Write content as a stream of tar entries
alonStaticContent :: MonadIO m
                  => HandleErrors
                  -> DirTree AnyContent
                  -> SourceT m Tar.Entry
alonStaticContent handleErrors contentDir =
  unfold dirTreeMinView contentDir ~> mkTarEntries
  where
    mkTarEntries = repeatedly $ do
      (path, content) <- await
      let entryPath = T.unpack $ T.intercalate "/" path
      yieldTarEntry handleErrors entryPath $ alonContentBody content
    -- Force the minView result into unfold parameters
    dirTreeMinView = fmap Tuple.swap . sequence . Tuple.swap .  LT.minView
    

-- | Tar archive entries for an nginx config
nginxConfig :: MonadIO m
            => HandleErrors
            -> DirTree NginxLocation
            -> ProcessT m a Tar.Entry
nginxConfig handleErrors nginxContent =
  construct $ yieldTarEntry handleErrors "nginx.conf" nginxConfigContents
  where
    nginxConfigContents = LBS.fromStrict . TE.encodeUtf8 $ renderNginxConfig nginxContent

-- | Yield a tar entry made from a filename and file content
yieldTarEntry :: MonadIO m
              => HandleErrors
              -> String
              -> LBS.ByteString
              -> PlanT k Tar.Entry m ()
yieldTarEntry handleErrors fp contents =
  either logTarError (\e -> yield e) $ mkTarEntry <$> Tar.toTarPath False ("htdocs/" <> fp)
  where
    mkTarEntry tarPath = Tar.fileEntry tarPath contents
    logTarError err =
      liftIO $ handleErrors [
          mconcat ["Error creating tar entry for ",  T.pack fp, ": ", T.pack err]
        ]

-- | A sink to write lazy ByteStrings to
-- after the writer is closed, the handle is closed
writeToHandle :: Handle -> ProcessT IO LBS.ByteString Void
writeToHandle h = handleWriter <> closeHandle
  where
    handleWriter = repeatedly $ liftIO . LBS.hPut h =<< await
    -- Close the handle, done after the handleWriter stops
    -- added because runSite doesn't terminate
    closeHandle = construct $ liftIO $ hClose h

-- | An nginx config location
data NginxLocation = NginxLocation {
  nginxHeaders :: HTTP.ResponseHeaders -- ^ List of headers to add to the location
, nginxStatus :: Maybe HTTP.Status -- ^ Optional status to return
} deriving (Eq, Ord, Show)

-- | Turn a piece of content into an nginx config location
toNginxLocationEntry :: AnyContent -> Maybe NginxLocation
toNginxLocationEntry content
  | requiresConfig = Just $ NginxLocation {
          nginxHeaders = headers
        , nginxStatus = if HTTP.statusCode status /= 200 then Just status else Nothing
      }
  | otherwise = Nothing
  where
    headers = alonContentHeaders content
    status = alonContentStatus content
    requiresConfig = or [
        HTTP.statusIsRedirection status
      , not (null headers)
      ]

renderNginxConfig :: DirTree NginxLocation -> T.Text
renderNginxConfig locations = do
  T.intercalate "\n" $ concat $ renderLocation <$> LT.toList locations
  where
    -- Render a single location as a list of lines
    renderLocation (path, location) =
      ["location " <> (T.intercalate "/" path) <> " {"]
      <> (indent . addHeader <$> nginxHeaders location)
      <> (maybe [] ((:[]) . addReturn) $ nginxStatus location)
      <> ["}" , ""]
    indent = flip T.append "  "
    addHeader :: HTTP.Header -> T.Text
    addHeader (name, value) =
      mconcat ["add_header ", TE.decodeUtf8 $ CI.original name, " ", TE.decodeUtf8 value, ";"] 
    addReturn status = mconcat ["return ", T.pack (show (HTTP.statusCode status)) ,";"]
