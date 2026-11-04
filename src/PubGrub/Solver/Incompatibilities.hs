{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module PubGrub.Solver.Incompatibilities (
  Incompatibilities',
  Incompatibilities,
  singleton,
  fromList,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Solver.Incompatibility (Incompatibility')
import PubGrub.Term (Term' (..))

type Incompatibilities provider =
  Incompatibilities' (DP.Package provider) (DP.VersionRange provider)

-- | A set of 'Incompatibility' values, indexed by package.
newtype Incompatibilities' package range
  = Incompatibilities
      ( HashMap
          package
          [Incompatibility' package range] -- Ordered newest -> oldest
      )

singleton :: (Hashable package) => Incompatibility' package range -> Incompatibilities' package range
singleton incompat = fromList [incompat]

fromList :: (Hashable package) => [Incompatibility' package range] -> Incompatibilities' package range
fromList = Incompatibilities . HashMap.fromListWith (<>) . concatMap indexed
 where
  indexed incompat =
    [ (term.package, [incompat])
    | term <- incompat.toList
    ]

-- | Get incompatibilities for the given package, ordered newest -> oldest.
instance
  (Hashable package) =>
  HasField
    "getOrdered"
    (Incompatibilities' package range)
    (package -> [Incompatibility' package range])
  where
  getField (Incompatibilities packages) package = HashMap.findWithDefault [] package packages
