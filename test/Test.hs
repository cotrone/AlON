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

import AlON.Source
import AlON.Transforms

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
tests = testGroup "AlON Tests" $
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

testAlONCase :: (Eq b, Show b)
             => TestName
             -> (forall t. (ReflexHost t) => Event t a -> HostFrame t (Dynamic t b))
             -> b -> [(Maybe a, Maybe b, b)]
             -> TestTree
testAlONCase tnm frm initVal cgen = testCase tnm $ eventTrace cgen initVal frm

sameBehavior :: (Reflex t, Eq a, MonadSample t m, MonadIO m, Show a) => Behavior t a -> Behavior t a -> m ()
sameBehavior ba bb = do
  va <- sample ba
  vb <- sample bb
  liftIO $ va @=? vb

testSelfTest :: TestTree
testSelfTest =
  testGroup "test self test" $
    [ testAlONCase "test testAlONCase empty" (const . return . constDyn $ 'a') 'a' []
    , testAlONCase "test testAlONCase Nothing" (const . return . constDyn $ 'a') 'a' [(Nothing, Nothing, 'a')]
    , testAlONCase "test testAlONCase hold" (holdDyn 'a') 'a'
        [ (Nothing, Nothing, 'a')
        , (Just 'b', Just 'b', 'b')
        , (Just 'c', Just 'c', 'c')
        , (Just 'a', Just 'a', 'a')
        ]
    ]

alonUpdateTests :: TestTree
alonUpdateTests =
    testGroup "AlON update tests" $
      [
      ]
