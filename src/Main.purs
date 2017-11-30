module Main where

import Control.Monad.Eff (Eff)
import Control.Semigroupoid ((>>>))
import Data.Array (range)
import Data.DotLang (class GraphRepr, toGraph)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.GenericGraph (class Edges, genericEdges, genericToGraph)
import Data.Semigroup ((<>))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Global (encodeURIComponent)
import Graphics.Graphviz (Engine(..), renderToSvg)
import Prelude hiding (div)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, img, pre, span)
import Text.Smolder.HTML.Attributes (src)
import Text.Smolder.Markup (text, (!), (#!))

--List
data List' a = Nil | Cons' a (List' a)

derive instance listGeneric :: Generic (List' a) _

instance listEdges :: Edges a => Edges (List' a) where edges x = genericEdges x

fromArray :: ∀a. Array a -> List' a
fromArray = foldr Cons' Nil

fromInt :: Int -> List' Int
fromInt = range 0 >>> fromArray


data Event = Increment | Decrement

type State = Int

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    div do
      button #! onClick (const Increment) $ text "Increment"
      span $ text (show count)
      button #! onClick (const Decrement) $ text "Decrement"
    div do
      img ! src ("data:image/svg+xml;charset=utf8," <> (encodeURIComponent $ renderToSvg Dot $ genericToGraph $fromInt count))

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
