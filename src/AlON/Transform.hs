{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts, RankNTypes, KindSignatures #-}
module AlON.Transform (
    timeGatedDir, parseGateTime
  , utf8DecodeDirTree
  , cacheTemplates
  , render
  ) where

import           AlON.Manipulation
import           AlON.Source
--import           AlON.Types
import           Control.Monad.Fix
import           Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict        as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import           Data.Time
import           Data.Witherable
import           Text.Mustache
import           Text.Mustache.Types
import           Text.Mustache.Compile
import           Text.Parsec.Error (ParseError, errorPos, messageString, errorMessages)
import           Reflex
import           Safe
import qualified System.FilePath as FP

-- | Take  TimeBits, and a DirTree. Assuming that the first path segment is a
--   UTCTime, encoded as ISO 8601, results in a DynDirTree pressenting only
--   elements who's key starts with a UTCTime not in the future of the pressent time.
--
--   Entries with invalid dates are dropped.
--
--   Examples of accepted ISO 8601 formats are:
--     - 2016-03-18 (Taken to mean the earliest time on said day)
--     - 2016-03-18T05:27:04 (Assumed to be Zulu)
--     - 2016-03-18T05:27:04Z
--     - 2016-03-18T05:27:04+00:00
--     - 2016-03-18T09:27:04+04:00
--     - 2016-W11-5 (Meaning the Friday of the 11th week of 2016, specificly 2016-03-18)
--     - 2016-W11-5T05:27:04
--     - 2016-W11-5T05:27:04Z
--     - 2016-W11-5T05:27:04+00:00
--
--   Once an entry has gone live, it should not stop being available.
--   The specific affect of this is that you may see an entry with an apparently future
--   time during a leap second where the clock jumps backwards.
timeGatedDir :: forall t m a
             . (Reflex t, MonadHold t m, MonadFix m)
             => TimeBits t -> DynDirTree t a -> m (DynDirTree t a)
timeGatedDir (dynTime, _) super = do
    pure $ filterTimes <$> dynTime <*> super
    --buildDynamic (trans =<< sample (current super)) $ pushAlways trans (updated super)
  where
    filterTimes :: UTCTime -> DirTree (Dynamic t a) -> DirTree (Dynamic t a)
    filterTimes t = LT.mapMaybeWithKey $ \el dc ->
                      case headMay el >>= parseGateTime of
                        Just tgtTime | t >= tgtTime -> Just dc
                        _ -> Nothing
    {-
    trans :: DirTree (Dynamic t a) -> PushM t (DirTree (Dynamic t a))
    trans = fmap LT.fromList . witherM filterTime . LT.toList
    filterTime :: ([Text], Dynamic t a) -> PushM t (Maybe ([Text], Dynamic t a))
    filterTime (ent@(el, _)) =
      case headMay el >>= parseGateTime of
        Just tgtTime -> do
          d <- afterTime tbs tgtTime >>= sample.current
          if d
            then pure $ Just ent
            else pure Nothing
        _ -> pure Nothing
    -}

parseGateTime :: T.Text -> Maybe UTCTime
parseGateTime t =
    headMay . mapMaybe (\f -> parseTimeM False defaultTimeLocale f s) $ timeFormats
  where
    s = T.unpack t
    timeFormats = (<>) <$> (weekFormats `mappend` dateFormats) <*> zoneFormats
    dateFormats = map iso8601DateFormat $
                  [ Just "%H:%M:%S"
                  ]
    weekFormats = [ "%Y-W%W-%wT%H:%M:%S"
                  , "%Y-%m-%d"
                  , "%Y-W%W-%w"
                  , "%Y-W%W-%w"
                  ]
    zoneFormats = [ "%Z", "%EZ", "" ]

render :: forall k t m
       . (ToMustache k, Reflex t, DynamicWriter t [Text] m)
       => Text -> Dynamic t TemplateCache -> Dynamic t k
       -> m (Dynamic t Text)
render nm t v = do
    let (errorResults, r) = splitDynPure $ applyTemplate <$> t <*> v
    tellDyn errorResults
    pure r
  where
    applyTemplate :: TemplateCache -> k -> ([Text], Text)
    applyTemplate tc actV =
     case HM.lookup (T.unpack nm) tc of
       Nothing -> (["Couldn't find template " `T.append` nm], mempty)
       Just tmpl -> (first $ map $ T.pack . show) . checkedSubstitute tmpl $ actV

utf8DecodeDirTree :: Functor (Dynamic t)
                  => DynDirTree t BS.ByteString -> DynDirTree t T.Text
utf8DecodeDirTree = apply2DynDirTree TE.decodeUtf8

cacheTemplates :: forall t m. (Monad m, Reflex t, DynamicWriter t [Text] m)
               => DynDirTree t T.Text -> m (Dynamic t TemplateCache)
cacheTemplates srcTree = do
    tellDyn errorResults
    return templCache
  where
    toMPath :: [Text] -> FilePath
    toMPath = FP.joinPath . fmap T.unpack
    compiledList :: Dynamic t [Dynamic t (Either ([Text], ParseError) Template)]
    compiledList =
     fmap (fmap (\(p, dt) -> (first ((,) p) . compileTemplate (toMPath p)) <$> dt) . LT.toList) $ srcTree

    splitLists :: Dynamic t ([([Text], ParseError)], [Template])
    splitLists = foldlDynDynList (\(a, b) -> either (\v -> (v:a, b)) (\v -> (a, v:b))) (constDyn ([], [])) compiledList

    errorList :: Dynamic t [([Text], ParseError)]
    errorList = fmap fst splitLists
    tmplList :: Dynamic t [Template]
    tmplList = fmap snd splitLists

    templCache :: Dynamic t TemplateCache
    templCache = fmap (flattenPartials . cacheFromList) tmplList

    flattenPartials :: TemplateCache -> TemplateCache
    flattenPartials m =
      HM.foldrWithKey (HM.insertWith (\_ b -> b { partials = HM.filterWithKey (\k _ -> k `elem` (getPartials $ ast b)) m } )) m m
   -- foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a Source
   -- insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v

    errorResults :: Dynamic t [Text]
    errorResults =
      fmap (fmap (\(p, e) -> mconcat
        [ T.intercalate "/" p, " : ", T.pack . show . errorPos $ e, " - "
        , T.intercalate "\n" . map (T.pack . messageString) . errorMessages $ e
        , "\n" ])) errorList
