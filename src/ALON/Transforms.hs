{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts, RankNTypes #-}
module ALON.Transforms (
    runProcess, RunExternal(..)
  , utf8DecodeDirTree
  , cacheTemplates
  , render
  ) where

import Control.Monad.Trans
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
import qualified System.Process as P
import System.Exit (ExitCode)
import GHC.IO.Handle

import System.IO.Unsafe

import Reflex
import ALON.Source
import ALON.Manipulation
import ALON.Types

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

render :: (Reflex t, ToMustache k, Functor (Dynamic t), Applicative (Dynamic t))
       => Text -> Dynamic t TemplateCache -> Dynamic t k
       -> Dynamic t Text
render nm t v =
    applyTemplate <$> t <*> v
  where
    applyTemplate tc actV =
     case HM.lookup (T.unpack nm) tc of
       Nothing -> "Couldn't find template " `T.append` nm
       Just tmpl -> substitute tmpl $ actV

utf8DecodeDirTree :: (Reflex t, Functor (Dynamic t))
                  => Dynamic t (DirTree (Dynamic t BS.ByteString)) -> Dynamic t (DirTree (Dynamic t T.Text))
utf8DecodeDirTree = apply2contents TE.decodeUtf8

flattenPartials :: TemplateCache -> TemplateCache
flattenPartials m = HM.foldrWithKey (HM.insertWith (\_ b -> b)) m m

cacheTemplates :: forall t m. (Reflex t, MonadALON t m, Functor (Dynamic t), Applicative (Dynamic t))
               => Dynamic t (DirTree (Dynamic t T.Text))
               -> m (Dynamic t TemplateCache)
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
    templCache = fmap (linkPartials . flattenPartials . cacheFromList) tmplList

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
