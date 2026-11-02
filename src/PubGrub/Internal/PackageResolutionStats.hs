{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.PackageResolutionStats (
  PackageResolutionStats (..),
  conflictCount,
) where

import GHC.Records (HasField (..))

-- | Statistics on how often a package conflicted with other packages.
--
-- Fields are not stable; 'conflictCount' is the only stable interface.
data PackageResolutionStats = PackageResolutionStats
  { unitPropagationAffected :: Int
  , unitPropagationCulprit :: Int
  , dependenciesAffected :: Int
  , dependenciesCulprit :: Int
  }

-- | The number of conflicts this package was involved in.
--
-- Processing packages with a high conflict count earlier usually speeds up resolution.
--
-- Whenever a package is part of the root cause incompatibility of a conflict, we increase its
-- count by one. Since the structure of the incompatibilities may change, this count too may
-- change in the future.
conflictCount :: PackageResolutionStats -> Int
conflictCount stats =
  sum
    [ stats.unitPropagationAffected
    , stats.unitPropagationCulprit
    , stats.dependenciesAffected
    , stats.dependenciesCulprit
    ]

instance HasField "conflictCount" PackageResolutionStats Int where
  getField = conflictCount
