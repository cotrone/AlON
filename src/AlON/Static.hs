{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
module AlON.Static where

import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Foldable
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

import AlON.ContentType.StaticFile
import AlON.Run

-- | A static file for 
data SiteFile = SiteFile {
  siteFileName :: T.Text
, siteFileContents :: BSL.ByteString
} deriving (Eq, Ord, Show)

-- Couldn't think of a better name
-- this is is warnings + errors from turning AlONContent
-- into a static file
data StaticFileException =
    ContentHasNoContentType
  | MimeTypeExtensionNotFound MIME.Type
  deriving (Eq, Ord, Show)

-- | Attempt to build a static file for a piece of AlONContent with a path
contentFile :: AlONContent a => ([T.Text], a) -> Either StaticFileException SiteFile
contentFile (fPath, content) = do
  mimeType <- maybe (Left ContentHasNoContentType) Right $ lookupAlONContentType content
  fExt <- maybe (Left $ MimeTypeExtensionNotFound mimeType) Right $ Map.lookup mimeType defaultFileExtensionMap
  pure $ SiteFile (mkFileName fExt) (alonContentBody content)
  where
    mkFileName ext = T.intercalate "/" fPath <> "." <> ext  

-- This is a little hacky, we could add a MIME type to AlONContent
lookupAlONContentType :: AlONContent a => a -> Maybe MIME.Type
lookupAlONContentType content = do
  (_, contentTypeHeader) <- Foldable.find ((== "Content-Type") . fst) $ alonContentHeaders content
  MIME.parseMIMEType $ TE.decodeUtf8 contentTypeHeader

runStatic :: AlONSite -> IO ()
runStatic site =
  runSite (TIO.putStrLn . T.intercalate "\n") startSite upSite site
  where
    upSite _ = pure ()
    startSite siteContent = do
      let allContent = contentFile <$> LT.toList siteContent
      pure ()
