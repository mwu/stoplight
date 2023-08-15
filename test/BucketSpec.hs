{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module BucketSpec where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import qualified Data.Atomics.Counter as AC
import           Data.Time.Clock as Time
import           Test.Hspec
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import qualified Stoplight.Bucket   as T
-------------------------------------------------------------------------------

spec_throttle :: Spec
spec_throttle = parallel $ describe "throttle" $ do
  forM_ [1000, 10000, 100000, 250000] $ \ tick ->
    forM_ [1, 5, 20, 40, 50] $ \ regen ->
      it ("should work at " ++ show (tick,regen)) $ do
        testThrottle tick regen
  where
    testThrottle tick regen =  do
        i <- AC.newCounter 0
        t <- T.new 0 50 tick regen

        replicateM_ 500 $ forkIO $ forever $ do
          T.wait t 1
          AC.incrCounter_ 1 i

        start <- Time.getCurrentTime
        -- wait a multiple of tick
        threadDelay (tick * 20)
        end <- Time.getCurrentTime

        let elapsedMicro = realToFrac (end `Time.diffUTCTime` start) * 1_000_000
        cnt <- AC.readCounter i

        let maxLim = (elapsedMicro / fromIntegral tick) * fromIntegral regen
            (cnt' :: Double) = fromIntegral cnt

        assertBool ("meets upperbound: " ++ show cnt') $ cnt' <= maxLim
        -- use a fudge factor of 70% to account for threadDelay delays
        assertBool ("meets lowerbound: " ++ show cnt') $ cnt' >= maxLim * 0.7


spec_overflow :: Spec
spec_overflow = parallel $ describe "overflow" $ do
  forM_ [1000, 10000, 100000, 250000] $ \ tick ->
    forM_ [1, 5, 20] $ \ regen ->
      it ("should not overflow at " ++ show (tick,regen)) $ do
        testOverflow tick regen
  where
    testOverflow tick regen = do
      t <- T.new 0 5 tick regen
      threadDelay (tick * 10)
      availableSlots <- T.peekAvail t
      assertBool "has the maximum reserve after two ticks" $ availableSlots == 5
