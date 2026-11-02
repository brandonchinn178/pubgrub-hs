{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub (
  resolve,

  -- * DependencyProvider
  DependencyProvider (..),
  IsVersionRange (..),
  Dependencies (..),

  -- ** PackageResolutionStats
  PackageResolutionStats,
  conflictCount,
) where

import Data.Map.Strict (Map)
import PubGrub.DependencyProvider (
  Dependencies (..),
  DependencyProvider (..),
  IsVersionRange (..),
  PackageResolutionStats,
  Version,
  conflictCount,
 )
import PubGrub.Error (Error)

-- | Finds a set of packages satisfying dependency bounds for a given package +
-- version pair.
--
-- It consists in efficiently finding a set of packages and versions that
-- satisfy all the constraints of a given project dependencies. In addition,
-- when that is not possible, PubGrub tries to provide a very human-readable and
-- clear explanation as to why that failed.
resolve ::
  (DependencyProvider provider) =>
  provider ->
  Package provider ->
  Version provider ->
  m (Either (Error provider) (Map (Package provider) (Version provider)))
resolve = undefined
