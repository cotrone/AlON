{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, FlexibleContexts, RankNTypes #-}
module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import qualified Control.Exception as E
import Data.Bits
import Data.Text ()
import Data.Time
import Data.Time.Clock.POSIX

import Test.Tasty
import Test.Tasty.HUnit

import Reflex.Host.Class
import Reflex.Test
import Data.Functor.Identity
import Data.Dependent.Map (DSum((:=>)))
import Control.Monad.Ref

import ALON.Source
import ALON.Transforms

import Reflex

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False err = E.throwIO . E.AssertionFailed $ err

main :: IO ()
main = do
    defaultMain tests

afterTimeSpec :: (Reflex t, MonadHold t m, MonadFix m)
              => TimeBits t -> UTCTime -> m (Dynamic t Bool)
afterTimeSpec (curTime, _) tgtTime = do
  n <- sample . current $ curTime
  case tgtTime <= n of
    True -> return . constDyn $ True
    False ->
      holdDyn False =<< (headE . ffilter (==True) . updated $ (((<) tgtTime) <$> curTime))

tests :: TestTree
tests = testGroup "ALON Tests" $
    [ parseTimeGroup
    , testSelfTest
    , alonUpdateTests
    ]

parseTimeGroup :: TestTree
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

testALONCase :: (Eq b, Show b)
             => TestName
             -> (forall t. (ReflexHost t) => Event t a -> HostFrame t (Dynamic t b))
             -> b -> [(Maybe a, Maybe b, b)]
             -> TestTree
testALONCase tnm frm initVal cgen = testCase tnm $ eventTrace cgen initVal frm

sameBehavior :: (Reflex t, Eq a, MonadSample t m, MonadIO m, Show a) => Behavior t a -> Behavior t a -> m ()
sameBehavior ba bb = do
  va <- sample ba
  vb <- sample bb
  liftIO $ va @=? vb

testSelfTest :: TestTree
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

alonUpdateTests :: TestTree
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
        , (Just . posixSecondsToUTCTime $ 4, Just True, True)
        , (Just . posixSecondsToUTCTime $ 5, Nothing, True)
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
        True
        [ (Just . posixSecondsToUTCTime $ 5, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 6, Nothing, True)
        , (Just . posixSecondsToUTCTime $ 3, Nothing, True)
        ]
      ]
