{-# LANGUAGE RankNTypes, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, TupleSections #-}
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Tuple as Tuple
import Data.Void
import qualified Network.HTTP.Types as HTTP
import System.IO

import AlON.Run
import AlON

-- | Build a static site bundle at the given filepath
staticizeSite :: HandleErrors -> ProcessT IO LBS.ByteString Void -> AlONSite -> IO ()
staticizeSite handleErrors tarSink site =
  runSite handleErrors startSite upSite site
  where
    upSite update = handleErrors ["Site was updated while generating: " <> T.pack (show update)]
    startSite siteContent = runT_ $ ((entrySource siteContent ~> tarWriter) <> finalizeTar) ~> tarSink
    entrySource siteContent =
      streamStaticContent handleErrors siteContent
      <> nginxConfig handleErrors (LT.mapMaybe toNginxLocationEntry siteContent)

streamStaticContent :: MonadIO m
                    => ([T.Text] -> IO ())
                    -> DirTree AnyContent
                    -> SourceT m Tar.Entry
streamStaticContent handleErrors siteContent =
  unfold dirTreeMinView siteContent ~> mkTarEntry
  where
    mkTarEntry = repeatedly $ handleTarError handleErrors . contentTarEntry =<< await
    contentTarEntry (path, content) = namedTarEntry (T.unpack $ T.intercalate "/" path) (alonContentBody content)
    dirTreeMinView = fmap Tuple.swap . sequence . Tuple.swap .  LT.minView

nginxConfig :: MonadIO m
            => ([T.Text] -> IO ())
            -> DirTree NginxLocationConfig
            -> (MachineT m k Tar.Entry)
nginxConfig handleErrors =
  construct . handleTarError handleErrors . mkNginxTarEntry . renderNginxConfig
  where
    mkNginxTarEntry = namedTarEntry "nginx.conf" . LBS.fromStrict . TE.encodeUtf8

handleTarError :: MonadIO m => ([T.Text] -> IO ()) -> Either String o -> PlanT k o m ()
handleTarError handleErrors =
  -- Lambda is here because plan has a hidden forall m 
  -- and it can't match the MonadIO m
  either logTarError (\e -> yield e)
  where
    logTarError err = liftIO $ handleErrors [T.pack err]

writeToHandle :: Handle -> ProcessT IO LBS.ByteString Void
writeToHandle h = repeatedly $ liftIO . LBS.hPut h =<< await

tarWriter :: ProcessT IO Tar.Entry LBS.ByteString
tarWriter = mapping $ \entry -> LBS.dropEnd (512 * 2) $ Tar.write [entry]

finalizeTar :: ProcessT IO Tar.Entry LBS.ByteString
finalizeTar = construct $ yield $ LBS.replicate (512*2) 0

namedTarEntry :: String -> LBS.ByteString -> Either String Tar.Entry
namedTarEntry fp contents = do
  tarPath <- Tar.toTarPath False fp
  pure $ Tar.fileEntry tarPath contents

data NginxLocationConfig = NginxLocationConfig {
  nginxHeaders :: HTTP.ResponseHeaders
, nginxStatus :: Maybe HTTP.Status
} deriving (Eq, Ord, Show)

type Indent = T.Text -> T.Text

toNginxLocationEntry :: AnyContent -> Maybe NginxLocationConfig
toNginxLocationEntry content
  | requiresConfig = Just $ NginxLocationConfig {
          nginxHeaders = alonContentHeaders content
        , nginxStatus = if HTTP.statusCode status /= 200 then Just status else Nothing
      }
  | otherwise = Nothing
  where
    status = alonContentStatus content
    requiresConfig = or [
        HTTP.statusIsRedirection (alonContentStatus content)
      , not (null (alonContentHeaders content))
      ]

renderNginxConfig :: DirTree NginxLocationConfig -> T.Text
renderNginxConfig = runNginxConfigRender renderCfg . renderNginxServerConfig
  where
    renderCfg = NginxRenderConfig {
        indentOnce = T.append "  "
      , indentAt = id
      }

data NginxRenderConfig = NginxRenderConfig {
  indentOnce :: T.Text -> T.Text
, indentAt :: T.Text -> T.Text
}

newtype NginxRenderM a = NginxRenderM {
  runNginxRenderM :: RWS NginxRenderConfig TB.Builder () a
} deriving (Functor, Applicative, Monad, MonadReader NginxRenderConfig, MonadWriter TB.Builder)

runNginxConfigRender :: NginxRenderConfig -> NginxRenderM () -> T.Text
runNginxConfigRender cfg (NginxRenderM renderCfg) =
  TL.toStrict $ TB.toLazyText $ snd $ evalRWS renderCfg cfg ()

writeLine :: T.Text -> NginxRenderM ()
writeLine line = do
  indentTo <- asks indentAt
  tell $ TB.fromText $ indentTo line
  tell $ "\n"

indent :: NginxRenderM () -> NginxRenderM ()
indent = local nextIndent
  where nextIndent cfg = cfg { indentAt = indentAt cfg . indentOnce cfg}

renderNginxServerConfig :: DirTree NginxLocationConfig -> NginxRenderM ()
renderNginxServerConfig locations = do
  writeLine "server {"
  indent $ do
    writeLine "root /www/data;"
    mapM_ renderLocation $ LT.toList locations
  writeLine "}"
  where
    renderLocation (path, location) = do
      writeLine $ "location " <> (T.intercalate "/" path) <> " {"
      indent $ do
        mapM_ addHeader $ nginxHeaders location
        maybe (pure ()) addReturn $ nginxStatus location
      writeLine "}"
      writeLine ""
    addHeader :: HTTP.Header -> NginxRenderM ()
    addHeader (name, value) =
      writeLine $ mconcat ["add_header ", TE.decodeUtf8 $ CI.original name, " ", TE.decodeUtf8 value, ";"] 
    addReturn status = writeLine $ mconcat ["return ", T.pack (show (HTTP.statusCode status)) ,";"]
