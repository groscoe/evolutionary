module Algorithm.Evolutionary.Operators.Selection (
  shuffleAndSelect,
  dropWeakest,
  fitnessProportionalByRoulette
  ) where

import Control.Monad.Random.Class
import Control.Monad (replicateM)
import System.Random.Shuffle (shuffleM)
import Data.List (sortOn)

import Algorithm.Evolutionary.Internals.Population

shuffleAndSelect :: (MonadRandom m, Real n) => (ind -> n) -> Int -> Population ind -> m [ind]
shuffleAndSelect fitnessFunction λ =
  fmap (take λ . sortOn fitnessFunction . take (2 * λ)) . shuffleM . getPopulation

dropWeakest :: (Applicative m, Real n) => (ind -> n) -> Int -> Population ind -> m [ind]
dropWeakest fitness numToDrop =
  pure . drop numToDrop . sortOn (negate . fitness) . getPopulation

fitnessProportionalByRoulette :: (MonadRandom m, Real n) => (ind -> n) -> Int -> Population ind -> m [ind]
fitnessProportionalByRoulette fitness λ population = replicateM λ . weighted $
  (\candidate -> (candidate, toRational $ fitness candidate)) <$> getPopulation population
