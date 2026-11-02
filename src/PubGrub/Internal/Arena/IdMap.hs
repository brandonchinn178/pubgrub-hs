{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.Arena.IdMap (
  IdMap,
  Id,
  new,
  insert,
  (!),
) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Prelude hiding (id)

data IdMap a = IdMap
  { values :: !(IntMap a)
  , size :: Int
  }

newtype Id a = Id Int
  deriving (Show, Eq, Ord)

new :: IdMap a
new = IdMap{values = IntMap.empty, size = 0}

insert :: a -> IdMap a -> (IdMap a, Id a)
insert a idmap = (idmap', Id id)
 where
  id = idmap.size
  idmap' =
    IdMap
      { values = IntMap.insert id a idmap.values
      , size = idmap.size + 1
      }

(!) :: (HasCallStack) => IdMap a -> Id a -> a
idmap ! (Id id) =
  -- Could happen if someone mixes up Ids from different IdMaps
  fromMaybe (error $ "IdMap failed to find Id: " <> show id) $
    IntMap.lookup id idmap.values
