module Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import Parent as Parent
import Prelude (Unit, bind, join, map, unit, void, ($))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.HTML.Window (document)

initialStore :: Unit
initialStore = unit

reduce :: forall a b. a -> b -> a
reduce store _ = store

main :: Effect Unit
main = do
  w <- window
  d <- document w
  maybe_elem <- getElementById "portal" (toNonElementParentNode d)
  let e2 :: Maybe HTMLElement
      e2 = join $ map fromElement maybe_elem
  HA.runHalogenAff do
    body <- HA.awaitBody
    
    -- Old halogen store fork
    {hoistedComponent} <- runStoreT initialStore reduce Parent.component
    let component = hoistedComponent

    -- New halogen store fork
    -- {component} <- runAndEmitStoreT initialStore reduce Parent.component

    -- Halogen store main line
    -- component <- runStoreT initialStore reduce Parent.component

    -- pure unit
    case e2 of
      Nothing -> liftEffect $ Console.error "portal elem not found"
      Just e  -> void $ runUI component {elem: e} body
