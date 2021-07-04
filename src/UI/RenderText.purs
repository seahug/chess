module Chess.UI.RenderText where

import Prelude

import Data.Array (intercalate)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Version (Version(..))

class RenderText a where
  renderText :: forall w i. a -> HTML w i

instance versionRenderText :: RenderText Version where
  renderText (Version major minor patch) = HH.text $ "v"
    <> (intercalate "." $ show <$> [ major, minor, patch ])

parenthesize :: forall w i. HTML w i -> Array (HTML w i)
parenthesize s = [ HH.text "(", s, HH.text ")" ]

