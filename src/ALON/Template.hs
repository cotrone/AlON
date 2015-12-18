{-# LANGUAGE ScopedTypeVariables #-}
module ALON.Template (
    cacheTemplates
  ) where

import Control.Monad
import qualified System.FilePath as FP
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Text.Mustache
import Text.Mustache.Types
import Text.Mustache.Compile
import Text.Parsec.Error (ParseError)

import Reflex
import ALON.Source
import ALON.Manipulation

--render :: String -> Dynamic t TemplateCache -> Dynamic t Value -> m (Dynamic t (Maybe Text))
--render t v = combineDyn 


dynBS2Text :: (Reflex t, MonadSample t m) => Dynamic t BS.ByteString -> m (Dynamic t Text)
dynBS2Text dt = (constDyn . TE.decodeUtf8) <$> (sample . current$ dt)

cacheTemplates :: forall t m. (Reflex t, MonadHold t m)
               => [Dynamic t (DirTree (Dynamic t BS.ByteString))]
               -> m (Dynamic t TemplateCache)
cacheTemplates srcs = do
  templateSrcTrees::[Dynamic t (DirTree (Dynamic t Text))] <-
                   mapM (mapDynM (mapM dynBS2Text)) srcs
  compiledTrees::[Dynamic t (DirTree (Dynamic t (Either ParseError Template)))] <-
                mapM (mapDynTreeWithKey (\p dt ->
                       (constDyn . compileTemplate (FP.joinPath . fmap T.unpack $ p))
                       <$> (sample . current $ dt)))
                     templateSrcTrees
  tmplList::Dynamic t [Dynamic t (Either ParseError Template)] <-
           mconcatDyn compiledTrees >>= mapDyn (map snd . LT.toList)
  (esD::Dynamic t [ParseError], tsD::Dynamic t [Template]) <- splitDyn =<<
     ((`mapDynM` tmplList) $ \tl ->
       foldM (\(es, ts) etd -> do
                  et <- sample . current $ etd
                  return $ case et of
                             Left pe -> (pe:es, ts)
                             Right t -> (es, t:ts))
        ([]::[ParseError], []::[Template]) tl)
  
  mapDyn cacheFromList tsD

--foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
