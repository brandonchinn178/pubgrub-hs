{-# LANGUAGE NoFieldSelectors #-}

module PubGrub (
  resolve,

  -- * DependencyProvider
  DependencyProvider (..),
  IsVersionSet (..),
  Dependencies (..),

  -- ** PackageResolutionStats
  PackageResolutionStats,
  conflictCount,
) where

import Data.Map (Map)
import PubGrub.DependencyProvider (
  Dependencies (..),
  DependencyProvider (..),
  IsVersionSet (..),
  PackageResolutionStats,
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
  DependencyProvider package versionSet incompat err m ->
  package ->
  version ->
  m (Either (Error package versionSet incompat err) (Map package version))
resolve provider package version = _
