-- | Provides utilities for optics.
--
-- @since 0.1
module Numeric.Data.Optics
  ( rmatching,
  )
where

import Optics.Core
  ( An_AffineTraversal,
    Is,
    NoIx,
    Optic,
    ReversibleOptic (..),
    matching,
  )

-- | Reversed 'matching'.
--
-- @since 0.1
rmatching ::
  (Is (ReversedOptic k) An_AffineTraversal, ReversibleOptic k) =>
  Optic k NoIx b a t s ->
  s ->
  Either t a
rmatching = matching . re
