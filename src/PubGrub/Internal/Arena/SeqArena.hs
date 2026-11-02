module PubGrub.Internal.Arena.SeqArena (
  SeqArena,
  empty,
  alloc,
  (!),
) where

import Data.Bifunctor (first)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import PubGrub.Internal.Arena.IdMap (Id, IdMap)
import PubGrub.Internal.Arena.IdMap qualified as IdMap
import Prelude hiding (id)

newtype SeqArena a = SeqArena (IdMap a)

empty :: SeqArena a
empty = SeqArena IdMap.new

alloc :: a -> SeqArena a -> (SeqArena a, Id a)
alloc a (SeqArena ids) = first SeqArena (IdMap.insert a ids)

(!) :: (HasCallStack) => SeqArena a -> Id a -> a
SeqArena ids ! id = withFrozenCallStack $ ids IdMap.! id
