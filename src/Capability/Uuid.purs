module Chess.Capability.GenUuid where

import Prelude

import Control.Monad.Trans.Class (lift)
import Chess.AppM (AppM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FFI as FFI
import Halogen (HalogenM)

class Monad m <= GenUuid m where
  genUuid :: m String

instance genUuidHalogenM :: GenUuid m => GenUuid (HalogenM st act slots msg m) where
  genUuid  = lift genUuid

instance genUuidAppM :: GenUuid AppM where
  genUuid = liftEffect FFI.genUuid

newtype GenUuidM a = GenUuidM (Effect a)

derive newtype instance functorGenUuidM :: Functor GenUuidM
derive newtype instance applyGenUuidM :: Apply GenUuidM
derive newtype instance applicativeGenUuidM :: Applicative GenUuidM
derive newtype instance bindGenUuidM :: Bind GenUuidM
derive newtype instance monadGenUuidM :: Monad GenUuidM
derive newtype instance monadEffectGenUuidM :: MonadEffect GenUuidM

instance genUuidGenUuidM :: GenUuid GenUuidM where
  genUuid = liftEffect FFI.genUuid

runGenUuidM :: GenUuidM ~> Effect
runGenUuidM (GenUuidM m) = liftEffect m

