{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Internal.PackageResolutionStats (
  PackageResolutionStats (..),
  conflictCount,
) where

import Data.Word (Word32)
import GHC.Records (HasField (..))

-- | Statistics on how often a package conflicted with other packages.
--
-- Fields are not stable; 'conflictCount' is the only stable interface.
data PackageResolutionStats = PackageResolutionStats
  { unitPropagationAffected :: Word32
  , unitPropagationCulprit :: Word32
  , dependenciesAffected :: Word32
  , dependenciesCulprit :: Word32
  }

-- | The number of conflicts this package was involved in.
--
-- Processing packages with a high conflict count earlier usually speeds up resolution.
--
-- Whenever a package is part of the root cause incompatibility of a conflict, we increase its
-- count by one. Since the structure of the incompatibilities may change, this count too may
-- change in the future.
conflictCount :: PackageResolutionStats -> Word32
conflictCount stats =
  sum
    [ stats.unitPropagationAffected
    , stats.unitPropagationCulprit
    , stats.dependenciesAffected
    , stats.dependenciesCulprit
    ]

instance HasField "conflictCount" PackageResolutionStats Word32 where
  getField = conflictCount
