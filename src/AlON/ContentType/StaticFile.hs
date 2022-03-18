{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module AlON.ContentType.StaticFile
 ( StaticFile(..)
 , staticize
 , mimeByFileName, mimeByFileName', defaultMimeMap
 , defaultFileExtensionMap
 ) where

import           AlON.Types
import           AlON.Manipulation
import qualified Codec.MIME.Type as MIME
import           Data.Bifunctor
import           Data.ByteString.Lazy (ByteString)
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.ListTrie.Patricia.Map.Ord (TrieMap)
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HTTP
import           Reflex
import           Reflex.Filesystem.DirTree
import qualified Data.Map as Map

data StaticFile
 = StaticFile
   { _fileHeaders :: HTTP.ResponseHeaders
   , _fileContent :: ByteString
   }
 deriving (Show, Eq, Ord)

instance AlONContent StaticFile where
  alonContentStatus _ = HTTP.ok200
  alonContentHeaders  = _fileHeaders
  alonContentBody     = _fileContent

staticize :: Functor (Dynamic t) => DynDirTree t ByteString -> DynDirTree t StaticFile
staticize = mapDynTreeWithKey (\pth dt -> StaticFile (pth2headers pth) dt)
  where
    pth2headers pth =
      let (mime, menc) = mimeByFileName pth
      in [("Content-Type", TE.encodeUtf8 . MIME.showType $ mime)]<>((("Content-Encoding",) . TE.encodeUtf8) <$> maybeToList menc)

type ContentEncoding = Text

mimeByFileName :: [Text] -> (MIME.Type, Maybe ContentEncoding)
mimeByFileName = mimeByFileName' defaultMimeMap

-- | Grabs the file extension list from the filename, and looks it up in the provided mime map.
mimeByFileName' :: TrieMap (CI Text) (MIME.Type, Maybe ContentEncoding) -> [Text] -> (MIME.Type, Maybe ContentEncoding)
mimeByFileName' mimeMap = fromMaybe (MIME.Type (MIME.Application "octet-stream") [], Nothing) . longestMatch mimeMap . map CI.mk . drop 1 . T.splitOn "." . last

longestMatch :: Ord k => TrieMap k a -> [k] -> Maybe a
longestMatch t ks =
    go ks
  where
    mt = Just <$> t
    go [] = Nothing
    go k = LT.lookupWithDefault (go (tail k)) k mt

-- | A default mime map derived by reference to:
--     - nginx's default mime map
--     - mime-supports mime.type file
--     - https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
--     - https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
defaultMimeMap :: TrieMap (CI Text) (MIME.Type, Maybe ContentEncoding)
defaultMimeMap = LT.fromList $ fmap (second (first (`MIME.Type` [])))
 [ (["html"], (MIME.Text "html", Nothing))
 , (["htm"],  (MIME.Text "html", Nothing))
 , (["css"],  (MIME.Text "css", Nothing))
 , (["xml"],  (MIME.Application "xml", Nothing))
 , (["mml"],  (MIME.Text "mathml", Nothing)) 
 , (["txt"],  (MIME.Text "plain", Nothing))
 , (["text"], (MIME.Text "plain", Nothing))
 , (["jad"],  (MIME.Text "vnd.sun.j2me.app-descriptor", Nothing))
 , (["ics"],  (MIME.Text "calendar", Nothing))
 , (["csv"],  (MIME.Text "csv", Nothing))
 , (["md"],   (MIME.Text "markdown", Nothing))
 , (["vcf"],  (MIME.Text "vcard", Nothing))
 , (["vcard"],(MIME.Text "vcard", Nothing))
 , (["bib"],  (MIME.Text "x-bibtex", Nothing))

 , (["gif"],  (MIME.Image "gif", Nothing))
 , (["jpeg"], (MIME.Image "jpeg", Nothing))
 , (["jpg"],  (MIME.Image "jpeg", Nothing))
 , (["jp2"],  (MIME.Image "jp2", Nothing))
 , (["jpg2"], (MIME.Image "jp2", Nothing))
 , (["png"],  (MIME.Image "png", Nothing))
 , (["tif"],  (MIME.Image "tiff", Nothing))
 , (["tiff"], (MIME.Image "tiff", Nothing))
 , (["wbmp"], (MIME.Image "vnd.wap.wbmp", Nothing))
 , (["ico"],  (MIME.Image "x-icon", Nothing))
 , (["jng"],  (MIME.Image "x-jng", Nothing))
 , (["bmp"],  (MIME.Image "x-ms-bmp", Nothing))
 , (["svg"],  (MIME.Image "svg+xml", Nothing))
 , (["svgz"], (MIME.Image "svg+xml", Just "gzip"))
 , (["webp"], (MIME.Image "webp", Nothing))

 , (["mid"],  (MIME.Audio "midi", Nothing))
 , (["midi"], (MIME.Audio "midi", Nothing))
 , (["kar"],  (MIME.Audio "midi", Nothing)) 
 , (["mp3"],  (MIME.Audio "mpeg", Nothing))
 , (["ogg"],  (MIME.Audio "ogg", Nothing))
 , (["m4a"],  (MIME.Audio "x-m4a", Nothing))
 , (["flac"], (MIME.Audio "flac", Nothing))
 , (["wav"],  (MIME.Audio "x-wav", Nothing))

 , (["3gp"],  (MIME.Video "3gpp", Nothing))
 , (["3gpp"], (MIME.Video "3gpp", Nothing))
 , (["ts"],   (MIME.Video "mp2t", Nothing))
 , (["mp4"],  (MIME.Video "mp4", Nothing))
 , (["mpeg"], (MIME.Video "mpeg", Nothing))
 , (["mpg"],  (MIME.Video "mpeg", Nothing))
 , (["webm"], (MIME.Video "webm", Nothing))
 , (["flv"],  (MIME.Video "x-flv", Nothing))
 , (["m4v"],  (MIME.Video "x-m4v", Nothing))
 , (["mng"],  (MIME.Video "x-mng", Nothing))
 , (["asx"],  (MIME.Video "x-ms-asf", Nothing))
 , (["asf"],  (MIME.Video "x-ms-asf", Nothing))
 , (["wmv"],  (MIME.Video "x-ms-wmv", Nothing))
 , (["avi"],  (MIME.Video "x-msvideo", Nothing))
 , (["ogv"],  (MIME.Video "ogg", Nothing))

 , (["otf"],  (MIME.Other "font" "otf", Nothing))
 , (["woff"], (MIME.Other "font" "woff", Nothing))
 , (["woff2"],(MIME.Other "font" "woff2", Nothing))
 , (["ttf"],  (MIME.Other "font" "ttf", Nothing))
 , (["pfr"],  (MIME.Application "font-tdpfr", Nothing))
 , (["eot"],  (MIME.Application "vnd.ms-fontobject", Nothing))
 
 , (["js"],   (MIME.Application "javascript", Nothing))
 , (["atom"], (MIME.Application "atom+xml", Nothing))
 , (["rss"],  (MIME.Application "rss+xml", Nothing))
 , (["jar"],  (MIME.Application "java-archive", Nothing))
 , (["war"],  (MIME.Application "java-archive", Nothing))
 , (["ear"],  (MIME.Application "java-archive", Nothing))
 , (["json"], (MIME.Application "json", Nothing))
 , (["jsonld"], (MIME.Application "ld+json", Nothing))
 , (["hqx"],  (MIME.Application "mac-binhex40", Nothing))
 , (["doc"],  (MIME.Application "msword", Nothing))
 , (["pdf"],  (MIME.Application "pdf", Nothing))
 , (["ps"],   (MIME.Application "postscript", Nothing))
 , (["eps"],  (MIME.Application "postscript", Nothing))
 , (["ai"],   (MIME.Application "postscript", Nothing))
 , (["rtf"],  (MIME.Application "rtf", Nothing))
 , (["m3u8"], (MIME.Application "vnd.apple.mpegurl", Nothing))
 , (["xls"],  (MIME.Application "vnd.ms-excel", Nothing))
 , (["ppt"],  (MIME.Application "vnd.ms-powerpoint", Nothing))
 , (["kml"],  (MIME.Application "vnd.google-earth.kml+xml", Nothing))
 , (["kmz"],  (MIME.Application "vnd.google-earth.kmz", Nothing))
 , (["7z"],   (MIME.Application "x-7z-compressed", Nothing))
 , (["cco"],  (MIME.Application "x-cocoa", Nothing))
 , (["jardiff"], (MIME.Application "x-java-archive-diff", Nothing))
 , (["jnlp"], (MIME.Application "x-java-jnlp-file", Nothing))
 , (["run"],  (MIME.Application "x-makeself", Nothing))
 , (["pl"],   (MIME.Application "x-perl", Nothing))
 , (["pm"],   (MIME.Application "x-perl", Nothing))
 , (["prc"],  (MIME.Application "x-pilot", Nothing))
 , (["pdb"],  (MIME.Application "x-pilot", Nothing))
 , (["rar"],  (MIME.Application "x-rar-compressed", Nothing))
 , (["rpm"],  (MIME.Application "x-redhat-package-manager", Nothing))
 , (["sea"],  (MIME.Application "x-sea", Nothing))
 , (["sit"],  (MIME.Application "x-stuffit", Nothing))
 , (["tcl"],  (MIME.Application "x-tcl", Nothing))
 , (["tk"],   (MIME.Application "x-tcl", Nothing))
 , (["der"],  (MIME.Application "x-x509-ca-cert", Nothing))
 , (["pem"],  (MIME.Application "x-x509-ca-cert", Nothing))
 , (["crt"],  (MIME.Application "x-x509-ca-cert", Nothing))
 , (["xpi"],  (MIME.Application "x-xpinstall", Nothing))
 , (["xhtml"],(MIME.Application "xhtml+xml", Nothing))
 , (["xht"],  (MIME.Application "xhtml+xml", Nothing))
 , (["xspf"], (MIME.Application "xspf+xml", Nothing))
 , (["zip"],  (MIME.Application "zip", Nothing))
 , (["bin"],  (MIME.Application "octet-stream", Nothing))
 , (["exe"],  (MIME.Application "octet-stream", Nothing))
 , (["dll"],  (MIME.Application "octet-stream", Nothing))
 , (["deb"],  (MIME.Application "vnd.debian.binary-package", Nothing))
 , (["ddeb"],  (MIME.Application "vnd.debian.binary-package", Nothing))
 , (["udeb"],  (MIME.Application "vnd.debian.binary-package", Nothing))
 , (["dmg"],  (MIME.Application "x-apple-diskimage", Nothing))
 , (["iso"],  (MIME.Application "octet-stream", Nothing))
 , (["img"],  (MIME.Application "octet-stream", Nothing))
 , (["msi"],  (MIME.Application "octet-stream", Nothing))
 , (["msp"],  (MIME.Application "octet-stream", Nothing))
 , (["msm"],  (MIME.Application "octet-stream", Nothing))
 , (["docx"], (MIME.Application "vnd.openxmlformats-officedocument.wordprocessingml.document", Nothing))
 , (["xlsx"], (MIME.Application "vnd.openxmlformats-officedocument.spreadsheetml.sheet", Nothing)) 
 , (["pptx"], (MIME.Application "vnd.openxmlformats-officedocument.presentationml.presentation", Nothing))
 , (["es"],   (MIME.Application "ecmascript", Nothing))
 , (["epub"], (MIME.Application "epub+zip", Nothing))
 , (["gz"],   (MIME.Application "gzip", Nothing))
 , (["bz"],   (MIME.Application "x-bzip", Nothing))
 , (["bz2"],  (MIME.Application "x-bzip2", Nothing))
 , (["pgp"],  (MIME.Application "pgp-encrypted", Nothing))
 , (["key"],  (MIME.Application "pgp-keys", Nothing))
 , (["sig"],  (MIME.Application "pgp-signature", Nothing))
 , (["rdf"],  (MIME.Application "rdf+xml", Nothing))
 , (["wasm"], (MIME.Application "wasm", Nothing))
 , (["apk"],  (MIME.Application "vnd.android.package-archive", Nothing))
 , (["tar"],  (MIME.Application "x-tar", Nothing))
 -- TODO: check that this downloads right.
 , (["tar", "gz"],  (MIME.Application "x-tar+gzip", Nothing))
 , (["xz"],  (MIME.Application "x-xz", Nothing))
 , (["ogx"],  (MIME.Application "ogg", Nothing))
 , (["latex"],  (MIME.Application "x-latex", Nothing))
 ]

-- | A map from MIME type to file extension
-- this could be made more explicit to ensure that
-- MIME types like html are html or htm specifically 
defaultFileExtensionMap :: Map.Map MIME.Type Text
defaultFileExtensionMap = Map.fromList $ fmap (\(fExts, (mimeType, _)) -> (mimeType, toFileExtension fExts)) $ LT.toList defaultMimeMap
  where
    toFileExtension = T.intercalate "." . fmap CI.original
