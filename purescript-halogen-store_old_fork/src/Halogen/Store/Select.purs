module Halogen.Store.Select where

import Prelude

import Unsafe.Reference (unsafeRefEq)

-- | A `Selector` represents a selection `a` from the store `store`. It is
-- | commonly used with the `connect` and `subscribe` functions when connecting
-- | a component to the store.
-- |
-- | A selector requires both a selection function from `store -> a` and an
-- | equality function for `a`. The equality function is used to make sure
-- | connected components are only notified when the selected state `a` has
-- | changed.
newtype Selector store a = Selector { eq :: a -> a -> Boolean, select :: store -> a }

-- | Create a `Selector` from an equality function and a function to select a
-- | sub-part of the store. The equality function will be used to determine if
-- | the selected state has changed.
select :: forall store a. (a -> a -> Boolean) -> (store -> a) -> Selector store a
select eq = Selector <<< { eq, select: _ }

-- | Create a `Selector` from a function to select a sub-part of the store. The
-- | selector will use the `Eq` class to determine if the selected state has
-- | changed.
selectEq :: forall store a. Eq a => (store -> a) -> Selector store a
selectEq = Selector <<< { eq, select: _ }

-- | Create a `Selector` for the entire store.
selectAll :: forall store. Selector store store
selectAll = Selector { eq: unsafeRefEq, select: identity }
