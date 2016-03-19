{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Bits
import Data.Text ()
import Data.Time
import Data.Time.Clock.POSIX
import Network.Wai.Handler.Warp (defaultSettings)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base (Assertion, (@=?))

import Reflex.Host.Class
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import Control.Monad.Ref

import ALON.Source
import ALON.Manipulation
import ALON.Run
import ALON.WebServer
import ALON.Transforms

import Reflex

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False err = E.throwIO . E.AssertionFailed $ err

main :: IO ()
main = do
    defaultMain tests
    runWarp defaultSettings frm
  where
    frm :: AlONSite
    frm = do
      et <- time 1
      let tbs = utc2TimeBits et
      now <- liftIO $ getCurrentTime
      tg <- afterTime tbs (5 `addUTCTime` now)
      dt <- dirSource "test_dir"
      mt' <- dirSource "math_dir"
      mt <- combineDyn (\tgb d -> if tgb then d else mempty) tg mt'
      let pt = mapDynTreeWithKey (\_ ds -> snd . runProcess $ (RunExternal "dc" [] ds)) mt
      rt <- mergeDynTree pt dt
      return rt

tests :: [Test]
tests =
    [ parseTimeGroup
    , testSelfTest
    , alonUpdateTests
    ]

parseTimeGroup :: Test
parseTimeGroup =
  testGroup "parseGateTime tests" $
    (map (\t -> testCase ("parse " ++ show t) (timeNonMidnight @=? parseGateTime t)) $
                      [ "2016-03-18T05:27:04"
                      , "2016-03-18T05:27:04Z"
                      , "2016-03-18T05:27:04+00:00"
                      , "2016-03-18T09:27:04+04:00"
                      , "2016-W11-5T05:27:04"
                      , "2016-W11-5T05:27:04Z"
                      , "2016-W11-5T05:27:04+00:00" ])
    `mappend` (map (\t -> testCase ("parse " ++ show t) (timeMidnight @=? parseGateTime t)) $
               [ "2016-03-18", "2016-W11-5" ])
  where
    timeNonMidnight = Just $ read "2016-03-18 05:27:04"
    timeMidnight = Just $ read "2016-03-18 00:00:00"

sameBehavior :: (Reflex t, Eq a, MonadSample t m, MonadIO m, Show a) => Behavior t a -> Behavior t a -> m ()
sameBehavior ba bb = do
  va <- sample ba
  vb <- sample bb
  liftIO $ va @=? vb

singleALON :: (Eq b, Show b)
           => [(Maybe a, Maybe b, b)] -> b -> (Event Spider a -> HostFrame Spider (Dynamic Spider b))
           -> Assertion
singleALON cases initVal frm = runSpiderHost $ do
  (re, rmt) <- newEventWithTriggerRef
  rd <- runHostFrame . frm $ re

  -- Make sure we initialized to the correct value
  actualStart <- sample . current $ rd
  liftIO $ initVal @=? actualStart

  -- Now check every step of our event list to make sure we get the right results.
  ehr <- subscribeEvent . updated $ rd
  mrt <- readRef rmt
  case mrt of
    Nothing -> return () -- Event isn't used in the test
    Just rt -> do
      void . forM cases $ \(ma, mb, b) -> do
        case ma of
          Nothing -> return ()
          Just a -> do
            stepEventValue <- fireEventsAndRead [rt :=> (Identity a)] $ readEvent ehr >>= sequence
            liftIO $ mb @=? stepEventValue
        afterStepValue <- sample . current $ rd
        liftIO $ b @=? afterStepValue

testALONCase :: (Eq b, Show b)
             => TestName -> (Event Spider a -> HostFrame Spider (Dynamic Spider b)) -> b -> [(Maybe a, Maybe b, b)]
             -> Test
testALONCase tnm frm initVal cgen = testCase tnm . singleALON cgen initVal $ frm

testSelfTest :: Test
testSelfTest =
  testGroup "test self test" $
    [ testALONCase "test testALONCase empty" (const . return . constDyn $ 'a') 'a' []
    , testALONCase "test testALONCase Nothing" (const . return . constDyn $ 'a') 'a' [(Nothing, Nothing, 'a')]
    , testALONCase "test testALONCase hold" (holdDyn 'a') 'a'
        [ (Nothing, Nothing, 'a')
        , (Just 'b', Just 'b', 'b')
        , (Just 'c', Just 'c', 'c')
        , (Just 'a', Just 'a', 'a')
        ]
    ]

alonUpdateTests :: Test
alonUpdateTests =
    testGroup "ALON update tests" $
      [ testALONCase "test utc2TimeBits" (\e -> do
                                             (t, tbs) <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 0) e
                                             -- We drop 12 because POSIX time is in terms of seconds.
                                             let dbs = sequenceA . take 28 . drop 12 $ tbs
                                             return $ (,) <$> t <*> dbs)
        (posixSecondsToUTCTime 0, take 28 . repeat $ 0) $
          let second1  = [1000000000000 `shiftR` b | b <- [12..39]]
              second2  = [2000000000000 `shiftR` b | b <- [12..39]]
              second60 = [60000000000000 `shiftR` b | b <- [12..39]]
          in [ (Nothing, Nothing
                       , (posixSecondsToUTCTime 0, take 28 . repeat $ 0))
             , (Just (posixSecondsToUTCTime 1), Just (posixSecondsToUTCTime 1, second1)
                                              , (posixSecondsToUTCTime 1, second1))
             , (Just (posixSecondsToUTCTime 2), Just (posixSecondsToUTCTime 2, second2)
                                              , (posixSecondsToUTCTime 2, second2))
             , (Just (posixSecondsToUTCTime 60), Just (posixSecondsToUTCTime 60, second60)
                                               , (posixSecondsToUTCTime 60, second60))
             ]
      , testALONCase "test afterTimeSpec" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 0) e
                                          afterTimeSpec tbs . posixSecondsToUTCTime $ 4)
        False
        [ (Nothing, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 1, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 4, Nothing, False) -- Why doesn't it fire here?
        , (Just . posixSecondsToUTCTime $ 5, Just True, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      , testALONCase "test afterTimeSpec skipping" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 0) e
                                          afterTimeSpec tbs . posixSecondsToUTCTime $ 4)
        False
        [ (Nothing, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 1, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 5, Just True, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      , testALONCase "test afterTimeSpec starting after" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 5) e
                                          afterTimeSpec tbs . posixSecondsToUTCTime $ 4)
        True
        [ (Just . posixSecondsToUTCTime $ 5, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      , testALONCase "test afterTime" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 0) e
                                          afterTime tbs . posixSecondsToUTCTime $ 4)
        False
        [ (Nothing, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 1, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 4, Nothing, False) -- Why doesn't it fire here?
        , (Just . posixSecondsToUTCTime $ 5, Just True, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      , testALONCase "test afterTime skipping" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 0) e
                                          afterTime tbs . posixSecondsToUTCTime $ 4)
        False
        [ (Nothing, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 1, Nothing, False)
        , (Just . posixSecondsToUTCTime $ 5, Just True, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      , testALONCase "test afterTimeSpec starting after" (\e -> do
                                          tbs <- utc2TimeBits <$> holdDyn (posixSecondsToUTCTime 5) e
                                          afterTime tbs . posixSecondsToUTCTime $ 4)
        False
        [ (Just . posixSecondsToUTCTime $ 5, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      ]
