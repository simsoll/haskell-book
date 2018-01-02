import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

-- Roll Your Own

-- 1.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where
     go :: Int -> Int -> StdGen -> Int
     go sum count gen
        | sum >= n = count
        | otherwise =
          let (die, nextGen) =
                randomR (1, 6) gen
          in go (sum + die)
                (count + 1) nextGen
