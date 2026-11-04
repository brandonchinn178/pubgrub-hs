{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module PubGrub.Solver.Monad (
  SolverM,
  runSolverM,
  SolverState (..),

  -- * API
  throwError,
  getState,
  setState,
  prioritize,
  chooseVersion,
  getDependencies,
  checkCancel,
  logger,
) where

import Data.Text (Text)
import GHC.Records (HasField (..))
import PubGrub.DependencyProvider (
  Dependencies,
  IsDependencyProvider,
  PackageResolutionStats,
 )
import PubGrub.DependencyProvider qualified as DP
import PubGrub.Error (PubGrubError (ShouldCancelError))
import PubGrub.Logger (Logger)
import PubGrub.Solver.Incompatibilities (Incompatibilities)
import PubGrub.Solver.PartialSolution (PartialSolution)

newtype SolverM provider a
  = SolverM
      ( forall r.
        (IsDependencyProvider provider) =>
        provider ->
        SolverState provider ->
        (PubGrubError provider -> DP.ResolveMonad provider r) ->
        (SolverState provider -> a -> DP.ResolveMonad provider r) ->
        DP.ResolveMonad provider r
      )

data SolverState provider = SolverState
  { partialSolution :: PartialSolution provider
  , incompats :: Incompatibilities provider
  }

runSolverM ::
  (IsDependencyProvider provider) =>
  provider ->
  SolverState provider ->
  SolverM provider a ->
  DP.ResolveMonad provider (Either (PubGrubError provider) a)
runSolverM provider state (SolverM run) = run provider state (pure . Left) (\_ -> pure . Right)

instance Functor (SolverM provider) where
  fmap f (SolverM run) = SolverM $ \provider s err next ->
    run provider s err (\s' a -> next s' (f a))
instance Applicative (SolverM provider) where
  pure x = SolverM $ \_ s _ next -> next s x
  SolverM runF <*> SolverM runX = SolverM $ \provider s0 err next ->
    runF provider s0 err $ \s1 f ->
      runX provider s1 err $ \s2 x ->
        next s2 (f x)
instance Monad (SolverM provider) where
  SolverM runA >>= k = SolverM $ \provider s0 err next ->
    runA provider s0 err $ \s1 a ->
      let SolverM runB = k a
       in runB provider s1 err next

lift :: DP.ResolveMonad provider a -> SolverM provider a
lift m = SolverM $ \_ s _ next -> m >>= next s

throwError :: PubGrubError provider -> SolverM provider a
throwError e = SolverM $ \_ _ err _ -> err e

getState :: SolverM provider (SolverState provider)
getState = SolverM $ \_ s _ next -> next s s

setState :: SolverState provider -> SolverM provider ()
setState s = SolverM $ \_ _ _ next -> next s ()

viaProvider :: ((IsDependencyProvider provider) => provider -> x) -> SolverM provider x
viaProvider f = SolverM $ \provider s _ next -> next s (f provider)

prioritize ::
  DP.Package provider ->
  DP.VersionRange provider ->
  PackageResolutionStats ->
  SolverM provider (DP.Priority provider)
prioritize package range stats = do
  action <- viaProvider DP.prioritize
  lift $ action package range stats

chooseVersion ::
  DP.Package provider ->
  DP.VersionRange provider ->
  SolverM provider (Either (DP.ResolutionError provider) (Maybe (DP.Version provider)))
chooseVersion package range = do
  action <- viaProvider DP.chooseVersion
  lift $ action package range

getDependencies ::
  DP.Package provider ->
  DP.Version provider ->
  SolverM provider (Either (DP.ResolutionError provider) (Dependencies provider))
getDependencies package version = do
  action <- viaProvider DP.getDependencies
  lift $ action package version

checkCancel :: SolverM provider ()
checkCancel = do
  action <- viaProvider DP.shouldCancel
  lift action >>= either (throwError . ShouldCancelError) pure

logger :: SolverM provider (GetLogger provider)
logger = GetLogger <$> viaProvider DP.logger

newtype GetLogger provider = GetLogger (Logger (DP.ResolveMonad provider))
instance HasField "debug" (SolverM provider (GetLogger provider)) (Text -> SolverM provider ()) where
  getField = getLogFunc (.debug)
instance HasField "info" (SolverM provider (GetLogger provider)) (Text -> SolverM provider ()) where
  getField = getLogFunc (.info)
instance HasField "warn" (SolverM provider (GetLogger provider)) (Text -> SolverM provider ()) where
  getField = getLogFunc (.warn)
instance HasField "error" (SolverM provider (GetLogger provider)) (Text -> SolverM provider ()) where
  getField = getLogFunc (.error)

getLogFunc ::
  (Logger (DP.ResolveMonad provider) -> Text -> DP.ResolveMonad provider ()) ->
  SolverM provider (GetLogger provider) ->
  Text ->
  SolverM provider ()
getLogFunc f getLogger message = do
  GetLogger l <- getLogger
  lift $ f l message
