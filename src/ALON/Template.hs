{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module ALON.Template (
    cacheTemplates
  , render
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
import Text.Parsec.Error (ParseError, errorPos, messageString, errorMessages)
import Data.Bifunctor
import qualified Data.HashMap.Strict    as HM

import Reflex
import ALON.Source
import ALON.Manipulation
import ALON.Types

render :: forall t m k. (Reflex t, MonadHold t m, ToMustache k)
       => Text -> Dynamic t TemplateCache -> Dynamic t k
       -> m (Dynamic t (Maybe Text))
render nm t v = do
  r <- (\f -> combineDyn f t v) $ \tc actV ->
    case HM.lookup (T.unpack nm) tc of
      Nothing -> ([mconcat ["Couldn't find template ", nm]], Nothing)
      Just tmpl -> ([], Just . substitute tmpl $ actV)
  (er, res) <- splitDyn r
  alonLogErrors er
  return res

dynBS2Text :: (Reflex t, MonadSample t m) => Dynamic t BS.ByteString -> m (Dynamic t Text)
dynBS2Text dt = (constDyn . TE.decodeUtf8) <$> (sample . current$ dt)

cacheTemplates :: forall t m. (Reflex t, MonadHold t m)
               => [Dynamic t (DirTree (Dynamic t BS.ByteString))]
               -> m (Dynamic t TemplateCache)
cacheTemplates srcs = do
  templateSrcTrees::[Dynamic t (DirTree (Dynamic t Text))] <-
                   mapM (mapDynM (mapM dynBS2Text)) srcs
  compiledTrees::[Dynamic t (DirTree (Dynamic t (Either ([Text], ParseError) Template)))] <-
                mapM (mapDynTreeWithKey (\p dt ->
                       (constDyn . first ((,) p) . compileTemplate (FP.joinPath . fmap T.unpack $ p))
                       <$> (sample . current $ dt)))
                     templateSrcTrees
  tmplList::Dynamic t [Dynamic t (Either ([Text], ParseError) Template)] <-
           mconcatDyn compiledTrees >>= mapDyn (map snd . LT.toList)
  (esD::Dynamic t [([Text], ParseError)], tsD::Dynamic t [Template]) <- splitDyn =<<
     ((`mapDynM` tmplList) $ \tl ->
       foldM (\(es, ts) etd -> do
                  et <- sample . current $ etd
                  return $ case et of
                             Left pe -> (pe:es, ts)
                             Right t -> (es, t:ts))
        ([]::[([Text], ParseError)], []::[Template]) tl)
  errD <- forDyn esD . map $ \(p, e) -> mconcat
            [ T.intercalate "/" p, " : ", T.pack . show . errorPos $ e, " - "
            , T.intercalate "\n" . map (T.pack . messageString) . errorMessages $ e
            , "\n" ]
  alonLogErrors errD
  mapDyn cacheFromList tsD

--foldDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
