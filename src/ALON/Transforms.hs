{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts, RankNTypes #-}
module ALON.Transforms (
    timeGatedDir, parseGateTime
  , runProcess, RunExternal(..)
  , utf8DecodeDirTree
  , cacheTemplates
  , render
  ) where

import Control.Monad.Trans
import Control.Monad.Fix
import qualified System.FilePath as FP
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ListTrie.Patricia.Map.Ord as LT
import Data.Time
import Data.Maybe
import Text.Mustache
import Text.Mustache.Types
import Text.Mustache.Compile
import Text.Parsec.Error (ParseError, errorPos, messageString, errorMessages)
import Data.Bifunctor
import qualified Data.HashMap.Strict    as HM
import qualified System.Process as P
import System.Exit (ExitCode)
import GHC.IO.Handle
import Safe

import System.IO.Unsafe

import Reflex
import ALON.Source
import ALON.Manipulation
import ALON.Types

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
              . (Reflex t, MonadHold t m, Functor (Dynamic t), MonadHold t (PullM t), MonadFix (PullM t))
             => TimeBits t -> DynDirTree t a -> m (DynDirTree t a)
timeGatedDir tbs super = do
    mapDynMHold (fmap LT.fromList . filterByTime) dynListTree
  where
    dynListTree :: Dynamic t [([Text], Dynamic t a)]
    dynListTree = LT.toList <$> super

    filterByTime :: (MonadSample t m', MonadHold t m', MonadFix m')
                 => [([Text], Dynamic t a)] -> m' [([Text], Dynamic t a)]
    filterByTime = fmap catMaybes . mapM filterElem
    filterElem :: (MonadSample t m', MonadHold t m', MonadFix m')
               => ([Text], Dynamic t a) -> m' (Maybe ([Text], Dynamic t a))
    filterElem el = do
      let mTime = headMay (fst el) >>= parseGateTime
      case mTime of
        Nothing -> return Nothing
        Just tgtTime -> do
          tg <- afterTime tbs tgtTime
          isTime <- sample . current $ tg
          return $ if isTime then Just el else Nothing 

parseGateTime :: T.Text -> Maybe UTCTime
parseGateTime t =
    headMay . mapMaybe (\f -> parseTimeM False defaultTimeLocale f s) $ timeFormats
  where
    s = T.unpack t
    timeFormats = weekFormats `mappend` dateFormats
    dateFormats = map iso8601DateFormat $
                  [ Just "%H:%M:%S%Z"
                  , Just "%H:%M:%S%z"
                  , Just "%H:%M:%S"
                  , Nothing ]
    weekFormats = [ "%Y-W%W-%wT%H:%M:%S%Z"
                  , "%Y-W%W-%wT%H:%M:%S%z"
                  , "%Y-W%W-%wT%H:%M:%S"
                  , "%Y-W%W-%w" ]

data RunExternal =
  RunExternal {
      reCmd :: FilePath
    , reArgs :: [String]
    , reStdIn :: BS.ByteString
    }
  deriving (Show, Eq)

{- stderr is directed to errors.
 -}
runProcess :: RunExternal -> (ExitCode, BS.ByteString)
runProcess (RunExternal cmd args indata) = unsafePerformIO $ do
  let cp = P.CreateProcess
        (P.RawCommand cmd args)
        Nothing Nothing
        P.CreatePipe P.CreatePipe P.CreatePipe True
        True False False False True Nothing Nothing
  (mcstdin, mcstdout, mcstderr, ph) <- liftIO . P.createProcess $ cp
  case (mcstdin, mcstdout, mcstderr) of
    (Just cstdin, Just cstdout, Just cstderr) -> do
      (exitCode, sout, serr) <- liftIO $ do
        BS.hPut cstdin indata
        hClose cstdin -- We're done sending the input, which we have to do before the process can exit.
        eCode_ <- P.waitForProcess ph
        sout_ <- BS.hGetContents cstdout
        hClose cstdout
        serr_ <- BS.hGetContents cstderr
        hClose cstderr
        return (eCode_, sout_, serr_)
      --unless (BS.null serr) $
      --  alonLogErrors . constDyn $ ["runProcess got errors from "<>(T.pack cmd)<>":", T.pack . show $ serr]
      -- Its perfectly reasonable for warnings to print to stderr and still have success.
      return (exitCode, sout)
    _ -> do
      --alonLogErrors . constDyn . pure $ "runProcess got Nothing for a handle."
      return undefined

render :: (ToMustache k, Applicative (Dynamic t))
       => Text -> Dynamic t TemplateCache -> Dynamic t k
       -> Dynamic t Text
render nm t v =
    applyTemplate <$> t <*> v
  where
    applyTemplate tc actV =
     case HM.lookup (T.unpack nm) tc of
       Nothing -> "Couldn't find template " `T.append` nm
       Just tmpl -> substitute tmpl $ actV

utf8DecodeDirTree :: (Functor (Dynamic t))
                  => DynDirTree t BS.ByteString -> DynDirTree t T.Text
utf8DecodeDirTree = apply2contents TE.decodeUtf8

cacheTemplates :: forall t m. (MonadALON t m, Applicative (Dynamic t))
               => DynDirTree t T.Text -> m (Dynamic t TemplateCache)
cacheTemplates srcTree = do
    alonLogErrors errorResults
    return templCache
  where
    toMPath :: [Text] -> FilePath
    toMPath = FP.joinPath . fmap T.unpack
    compiledList :: Dynamic t [Dynamic t (Either ([Text], ParseError) Template)]
    compiledList =
     fmap (fmap (\(p, dt) -> (first ((,) p) . compileTemplate (toMPath p)) <$> dt) . LT.toList) $ srcTree

    errorList :: Dynamic t [([Text], ParseError)]
    errorList = foldlDynDynList (\cache -> either (:cache) (const cache)) (constDyn []) compiledList
    tmplList :: Dynamic t [Template]
    tmplList = foldlDynDynList (\cache -> either (const cache) (:cache)) (constDyn []) compiledList

    templCache :: Dynamic t TemplateCache
    templCache = fmap (linkPartials . cacheFromList) tmplList

    linkPartials :: TemplateCache -> TemplateCache
    linkPartials tc0 =
      let tc = fmap (\t -> t {partials = tc}) tc0
      in tc

    errorResults :: Dynamic t [Text]
    errorResults =
      fmap (fmap (\(p, e) -> mconcat
        [ T.intercalate "/" p, " : ", T.pack . show . errorPos $ e, " - "
        , T.intercalate "\n" . map (T.pack . messageString) . errorMessages $ e
        , "\n" ])) errorList
