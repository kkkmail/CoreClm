namespace ClmSys

module DistributionData =

    /// Describes how to resolve collisions when sampling from a large set of elements.
    ///     1. NoCollisionResolution - just sample from the distribution.
    ///     2. ExcludeDuplicates - make sure that there are no duplicates in the sample.
    type CollisionResolutionType =
        | NoCollisionResolution
        | ExcludeDuplicates
