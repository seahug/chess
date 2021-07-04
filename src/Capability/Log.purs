module Chess.Capability.Log where

import Prelude

import Control.Monad.Trans.Class (lift)
import Chess.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (HalogenM)

class Monad m <= Log m where
  log :: String -> m Unit
  error :: String -> m Unit

instance logHalogenM :: Log m => Log (HalogenM st act slots msg m) where
  log  = lift <<< log
  error  = lift <<< error

instance logAppM :: Log AppM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

newtype LogM a = LogM (Effect a)

derive newtype instance functorLogM :: Functor LogM
derive newtype instance applyLogM :: Apply LogM
derive newtype instance applicativeLogM :: Applicative LogM
derive newtype instance bindLogM :: Bind LogM
derive newtype instance monadLogM :: Monad LogM
derive newtype instance monadEffectLogM :: MonadEffect LogM

instance logLogM :: Log LogM where
  log = liftEffect <<< Console.log
  error = liftEffect <<< Console.error

runLogM :: LogM ~> Effect
runLogM (LogM m) = liftEffect m

