{-# LANGUAGE OverloadedStrings #-}
module Kashmir.Snap.Snaplet.Random (getRandom,initRandom,RandomNumberGenerator) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Snap
import           System.Random

newtype RandomNumberGenerator = RandomNumberGenerator (TVar StdGen)

popRandom :: Random a => RandomNumberGenerator -> STM a
popRandom (RandomNumberGenerator tvar) =
  do rnd <- readTVar tvar
     let (x,rnd') = random rnd
     writeTVar tvar rnd'
     return x

getRandom :: Random a => Handler b RandomNumberGenerator a
getRandom = get >>= liftIO . atomically . popRandom

initRandom :: SnapletInit a RandomNumberGenerator
initRandom =
  makeSnaplet "random-number-generator" "A source of randomness." Nothing .
  liftIO $
  do stdGen <- getStdGen
     tvar <- atomically $ newTVar stdGen
     return $
       RandomNumberGenerator tvar
