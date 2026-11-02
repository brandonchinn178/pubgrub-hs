module PubGrub.Internal.PartialSolution (
  PartialSolution,
  empty,

  -- * DecisionLevel
  DecisionLevel,
) where

import Data.Word (Word32)

-- | The partial solution contains all package assignments, organized by package
-- and historically ordered.
data PartialSolution package versionSet incompat priority = PartialSolution
  {}

empty :: PartialSolution package versionSet incompat priority
empty = PartialSolution

--   { next_global_index :: !Int
--   , current_decision_level :: !DecisionLevel
--   -- ^ The number of decisions that have been made, equal to the number of
--   -- packages with decisions.
--   , package_assignments :: !(Map (Id package) (PackageAssignments package versionSet incompat)) -- TODO: FnvIndexMap?
--
--   -- ^ Store for all known package decisions and package derivations.
--   --
--   -- "assignment" refers to both packages with decisions and package with only derivations and
--   -- no decision yet. We combine this in a single index map with a decisions section in front and
--   -- a derivations sections in the back, where making a decision moves a package from the
--   -- derivations sections to the decisions section.
--   --
--   -- `[..current_decision_level]`: Packages that have had a decision made, sorted by the
--   -- `decision_level`. The section is can be seen as the partial solution, it contains a
--   -- mapping from package name to decided version. The sorting makes it very efficient to
--   -- extract the solution, and to backtrack to a particular decision level. The
--   -- `AssignmentsIntersection` is always a `Decision`.
--   --
--   -- `[current_decision_level..]`: Packages that are dependencies of some other package,
--   -- but have not yet been decided. The `AssignmentsIntersection` is always a `Derivations`, the
--   -- derivations store the obligations from the decided packages.
--   , prioritized_potential_packages :: _ -- PriorityQueue<Id<DP::P>, (DP::Priority, Reverse<u32>), BuildHasherDefault<FxHasher>>,
--
--   -- ^ The undecided packages order by their `Priority`.
--   --
--   -- The max heap allows quickly `pop`ing the highest priority package.
--   --
--   -- The `Reverse<u32>` is the discovery order of packages used as tiebreaker. Its order is that
--   -- of a breadth-first search.
--   , outdated_priorities :: _ -- FnvIndexSet<Id<DP::P>>,
--
--   -- ^ Packages whose derivations changed since the last time `prioritize` was called and need
--   -- their priorities to be updated.
--   , last_valid_decision_levels :: [DecisionLevel]
--   -- ^ For each completed backtrack generation, the highest decision level that remains valid.
--   -- The active generation is represented by the missing entry after the end of this vector.
--   }

newtype DecisionLevel = DecisionLevel Word32
  deriving (Show, Eq, Ord, Bounded)
