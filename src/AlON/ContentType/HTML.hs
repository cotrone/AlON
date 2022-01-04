{-# LANGUAGE OverloadedStrings #-}
module AlON.ContentType.HTML
 ( HTMLPage(..), htmlize
 ) where

import           AlON.Types
import           Data.ByteString.Lazy (ByteString)
import qualified Codec.MIME.Type as MIME
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HTTP
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

data HTMLPage
 = HTMLPage
   { _htmlHeaders :: HTTP.ResponseHeaders
   , _htmlContent :: ByteString
   }
 deriving (Read, Show, Eq, Ord)

htmlize :: Blaze.Html -> HTMLPage
htmlize = HTMLPage [] . Blaze.renderHtml

instance AlONContent HTMLPage where
  alonContentStatus  _ = HTTP.ok200
  alonContentHeaders c = ("Content-Type", TE.encodeUtf8 . MIME.showType $ MIME.Type (MIME.Text "html") [MIME.MIMEParam "charset" "UTF-8"]):_htmlHeaders c
  alonContentBody      = _htmlContent
