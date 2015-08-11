module Kashmir.Snap.Snaplet.Random (popRandom,initRandom,RandomNumberGenerator) where

import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Tuple
import           Snap
import           System.Random

newtype RandomNumberGenerator = RandomNumberGenerator (IORef StdGen)

popRandom :: Random a => RandomNumberGenerator -> IO a
popRandom (RandomNumberGenerator io) = atomicModifyIORef io (swap . random)

initRandom :: SnapletInit a RandomNumberGenerator
initRandom =
  makeSnaplet "random-number-generator" "A source of randomness." Nothing .
  liftIO $
  do stdGen <- getStdGen
     ioRef <- newIORef stdGen
     return $
       RandomNumberGenerator ioRef
