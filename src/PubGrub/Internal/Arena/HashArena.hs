{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.Arena.HashArena (
  HashArena,
  empty,
  alloc,
  (!),
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import PubGrub.Internal.Arena.IdMap (Id, IdMap)
import PubGrub.Internal.Arena.IdMap qualified as IdMap
import Prelude hiding (id)

data HashArena a = HashArena
  { ids :: !(HashMap a (Id a))
  , values :: !(IdMap a)
  }

empty :: HashArena a
empty = HashArena{ids = HashMap.empty, values = IdMap.new}

alloc :: (Hashable a) => a -> HashArena a -> (HashArena a, Id a)
alloc a arena =
  let ((values', id), ids') =
        HashMap.alterF
          ( fmap Just . \case
              Just id_ -> ((arena.values, id_), id_)
              Nothing ->
                let (values_, id_) = IdMap.insert a arena.values
                 in ((values_, id_), id_)
          )
          a
          arena.ids
   in (arena{ids = ids', values = values'}, id)

(!) :: (HasCallStack) => HashArena a -> Id a -> a
arena ! id = withFrozenCallStack $ arena.values IdMap.! id
