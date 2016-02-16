{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, FlexibleContexts, RankNTypes #-}
module ALON.Transforms (
    runProcess, RunExternal(..)
  , cacheTemplates
  , render
  ) where

import Control.Monad
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

mapDynMIO :: forall t m a b. (Reflex t, MonadHold t m, MonadIO (PushM t), MonadIO (PullM t)) => (forall m'. (MonadSample t m', MonadIO m') => a -> m' b) -> Dynamic t a -> m (Dynamic t b)
mapDynMIO f d = do
  let e' = push (liftM Just . f :: a -> PushM t (Maybe b)) $ updated d
      eb' = fmap constant e'
      v0 = pull $ f =<< sample (current d)
  bb' :: Behavior t (Behavior t b) <- hold v0 eb'
  let b' = pull $ sample =<< sample bb'
  return $ unsafeDynamic b' e'

{- stderr is directed to errors.
 -}
runProcess :: (Reflex t, MonadHold t m, MonadIO (PushM t), MonadIO (PullM t)) => Dynamic t RunExternal -> m (Dynamic t (ExitCode, BS.ByteString))
runProcess = mapDynMIO $ \(RunExternal cmd args indata) -> do
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
      return $ (exitCode, sout)
    _ -> do
      -- alonLogErrors . constDyn . pure $ "runProcess got Nothing for a handle."
      return undefined

render :: forall t m k. (Reflex t, MonadALON t m, ToMustache k)
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

cacheTemplates :: forall t m. (Reflex t, MonadALON t m)
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

{-
apply2contents :: (Reflex t, MonadHold t m) => (Event t a -> Event t b) -> Dynamic t (DirTree (Dynamic t a)) -> m (Dynamic t (DirTree (Dynamic t b)))
apply2contents trans = mapDynM $
  traverse . fmap (\n -> do
           c <- sample . current $ n
           trans . updated $ n)
-}
