{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.AddedDependencies (
  AddedDependencies,
  new,
  insert,
) where

import Data.Foldable qualified as Seq (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import PubGrub.Internal.Arena.IdMap (Id)

-- | Tracks package versions whose dependency incompatibilities have already
-- been added.
--
-- Before the first backtrack, entries are stored in a compact history because
-- decisions cannot be revisited. The history is promoted to a deduplicating map
-- only once revisits become possible.
data AddedDependencies package version = AddedDependencies
  { beforeFirstBacktrack :: !(Seq (Id package, version))
  , afterFirstBacktrack :: !(Maybe (Map (Id package) (Set version)))
  }

new :: AddedDependencies package version
new =
  AddedDependencies
    { beforeFirstBacktrack = Seq.empty
    , afterFirstBacktrack = Nothing
    }

insert ::
  (Ord version) =>
  Id package ->
  version ->
  Bool ->
  AddedDependencies package version ->
  (AddedDependencies package version, Bool)
insert package version hasBacktracked addedDeps
  | not hasBacktracked =
      let addedDeps' =
            addedDeps
              { beforeFirstBacktrack = addedDeps.beforeFirstBacktrack Seq.|> (package, version)
              }
       in (addedDeps', True)
  | otherwise =
      let (packages, addedDeps') =
            case addedDeps.afterFirstBacktrack of
              Nothing ->
                ( fromBeforeFirstBacktrack addedDeps.beforeFirstBacktrack
                , addedDeps{beforeFirstBacktrack = Seq.empty}
                )
              Just pkgs -> (pkgs, addedDeps)
          (isNew, packages') = insertDepReturnIsNew package packages
       in (addedDeps'{afterFirstBacktrack = Just packages'}, isNew)
 where
  fromBeforeFirstBacktrack = fmap Set.singleton . Map.fromList . Seq.toList
  insertDepReturnIsNew =
    Map.alterF
      ( fmap Just . \case
          Nothing -> (True, Set.singleton version)
          Just versions -> insertSetReturnIsNew version versions
      )
  insertSetReturnIsNew = Set.alterF (\present -> (not present, True))
