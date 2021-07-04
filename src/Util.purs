module Util where

import Prelude

import Data.Argonaut (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, take, (:))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (any, elem, notElem)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens.Getter (view)
import Data.Lens.Lens (Lens')
import Data.Lens.Setter (over)
import Data.Tuple (Tuple(..), fst)

dropIndex :: forall a. Int -> Array a -> Array a
dropIndex i xs = take i xs <> drop (i + 1) xs

indices :: forall a. Array a -> Array Int
indices xs = fst <$> mapWithIndex Tuple xs

type ArrayLens' s a = Lens' s (Array a)

prependOver :: forall s a. ArrayLens' s a -> a -> s -> s
prependOver lens x = over lens (x : _)

infixr 4 prependOver as :~

readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap show <<< decodeJson <=< jsonParser

writeJson :: forall a. EncodeJson a => a -> String
writeJson = stringify <<< encodeJson

