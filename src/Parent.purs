module Parent where

import Child as Child
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.Portal as Portal
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, StoreT)
import Halogen.Store.Select (selectAll)
import Prelude (Unit, Void, discard, unit, ($), (=<<))
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement (HTMLElement)


data Action
  = HandleChild Child.Output

type ChildSlots
  = ( child :: H.Slot Child.Query Child.Output Unit
    )

-- type State = Unit
type State =
  { elem :: HTMLElement
  }

type StoreAction = Unit
type Store = Unit

type AppMonad = StoreT StoreAction Store Aff

_child :: Proxy "child"
_child = Proxy

component :: H.Component (Const Void) State Void AppMonad
component = connect selectAll $ H.mkComponent
    { initialState: \{input} -> input
    , render: render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }

handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output AppMonad Unit
handleAction action = case action of
  HandleChild output -> case output of
    -- We can receive messages directly from the child, as usual
    Child.Clicked -> do
      Console.log "clicked"
      -- and we can also query the child directly, as usual
      traverse_ Console.logShow =<< H.request _child unit Child.GetCount

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render {elem} =
  HH.div
    []
    [ HH.text "I'm the parent"
    -- This is almost identical to using the `slot` function, but this component
    -- will _not_ be rendered within the parent component <div> in the DOM.
    , Portal.portalAff _child unit Child.component unit (Just elem) (HandleChild)
    ]