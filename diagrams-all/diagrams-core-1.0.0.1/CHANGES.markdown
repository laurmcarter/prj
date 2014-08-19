1.0.0.1 (27 November 2013)
--------------------------

    - Allow semigroups-0.12

1.0 (25 November 2013)
----------------------

* **New features**

    * Delayed subtrees: instead of a primitive, one can now also have
      a delayed subtree at a leaf, containing a continuation which
      generates a `QDiagram` when given the accumulated d-annotation
      at that point in the tree.  Useful for things which need to know
      the final transformation applied to them before deciding what
      diagram to generate.  The prototypical use case is arrows: see
      https://github.com/diagrams/diagrams-lib/issues/112 .  However,
      this may be useful for other things as well: for example,
      diagrams which scale normally until hitting some maximum or
      minimum size, at which point they refuse to scale any further
      (or more generally diagrams which scale as some non-linear
      function of the transformation applied to them).

      The only downside is that the u-annotation must be fixed ahead
      of time---doing otherwise requires a more general solution for
      constraint solving.

    * New function `lookupName` for doing a simple lookup of a named
      subdiagram

    * New module `Diagrams.Core.Compile`, containing a framework for
      compiling `QDiagrams` into a simpler tree type `RTree`, which
      may be used by backends for rendering.

* **New instances**

    * `Qualifiable` instances for `(,)`, `(,,)`, `[]`, `Set`, `Map k`,
      and `(->) e`.

    * `(->) e` instance for `Juxtaposable` (thanks to Carlos Scheidegger)

* **API changes**

    * Export `pointDiagram` function, which creates an otherwise empty
      diagram with a point (not empty) envelope

    * A bunch of stuff now uses machinery from the `lens` library.
	    * `envelope`, `trace`, and `subMap` are now `Lens'`es
        * `Wrapped` instances for `Trace`, `TransInv`, `QDiagram`,
          `SubMap`, `Envelope`, `Style`, `Query`, and `Name` (replaces
          `Newtype` instances)
	    * `Iso`s for `Query`, `Envelope`, `QDiagram`, `SubMap`, `TransInv`

0.7.0.1 (26 September 2013)
---------------------------

    allow semigroups-0.11

0.7: 9 August 2013
------------------

* **New features**

    - new function `onBasis`, to extract the matrix equivalent of a `Transformation`
    - `SubMap`s are now `Deletable`
    - new function `localize` for hiding/deleting names from scope
    - new `IsPrim` class, containing `transformWithFreeze` function.
        This is primarily intended to support scale-invariant primitives
        (*e.g.* arrowheads) but may be useful for other stuff as well.
	The default implementation of `renderDia` now uses
	`transformWithFreeze`.
    - optimized `Transformable` instance for `TransInv`

* **New instances**

    - `Eq`, `Ord`, `Enveloped`, `Traced`, and `Qualifiable` instances
      for `TransInv`

    - `Transformable` instance for functions, which acts by conjugation

* **API changes**

    - `named` and `namePoint` have moved to the `diagrams-lib` package.

* **Dependency/version changes**

    - allow `base-4.7`
    - upgrade to `monoid-extras-0.3`

0.6.0.2: 5 March 2013
---------------------

* bug fix: the 'diameter' and 'radius' functions now work correctly.

0.6.0.1: 7 January 2013
-----------------------

* allow `semigroups-0.9`

0.6: 11 December 2012
---------------------

* **New features**

    - Proper support for subdiagrams: previous versions of
      diagrams-core had a mechanism for associating names with a pair
      of a location and an envelope.  Now, names are associated with
      actual subdiagrams (including their location and envelope, along
      with all the other information stored by a diagram).

        See
        [`Diagrams.Core.Types`](https://github.com/diagrams/diagrams-core/blob/27b275f45cad514caefcd3035e4e261f1b4adf6f/src/Diagrams/Core/Types.hs#L493).

    - Traces: in addition to an envelope, each diagram now stores a
      "trace", which is like an embedded raytracer: given any ray
      (represented by a base point and a vector), the trace computes
      the closest point of intersection with the diagram along the
      ray.  This is useful for determining points on the boundary of a
      diagram, *e.g.* when drawing arrows between diagrams.

        See [`Diagrams.Core.Trace`](https://github.com/diagrams/diagrams-core/blob/2f8727fdfa60cdf46456a23f358c8a771b2cd90d/src/Diagrams/Core/Trace.hs).

* **API changes**

    - The modules have all been renamed to be more consistent with the
      module naming scheme in the rest of the diagrams universe.  In
      particular:

        `Graphics.Rendering.Diagrams`       -->  `Diagrams.Core`
        `Grahpics.Rendering.Diagrams.Core`  -->  `Diagrams.Core.Types`
        `Graphics.Rendering.Diagrams.*`     -->  `Diagrams.Core.*`

    - `Graphics.Rendering.Diagrams.UDTree` has been split out into a
      separate
      [`dual-tree`](http://hackage.haskell.org/package/dual%2Dtree)
      package (which has also been substantially rewritten).

    - `Graphics.Rendering.Diagrams.{Monoids,MList}` have been split
      out into a separate [`monoid-extras`](http://hackage.haskell.org/package/monoid%2Dextras) package.

    - The `names` function now returns a list of names and their
      associated locations, instead of the associated subdiagrams.  In
      particular the output is suitable to be rendered to a `String`
      using `show`.

    - The new `subMap` function fills a similar role that `names` used
      to play, returning the entire mapping from names to subdiagrams.

    - New functions `envelope[VP]May`

        `envelopeV` and `envelopeP` return the zero vector and origin,
        respectively, when called on an empty envelope.  However,
        sometimes it's useful to actually know whether the envelope was
        empty or not (the zero vector and the origin are legitimate
        outputs from non-empty envelopes).  The new functions have their
        return type wrapped in `Maybe` for this purpose.

    - New functions `envelopeS` and `envelopeSMay`

        Like `envelope[VP](May)`, but returning a scalar multiple of
		the input vector.

    - The `Graphics.Rendering.Diagrams.Util` module has been removed,
      along with the `withLength` function.  Calls to `withLength` can
      be replaced using

        `withLength s v = s *^ normalized v`

    - Add needed constraints `(InnerSpace v, OrderedField (Scalar v),
      Monoid' m)` to the type of the `renderDias` method in the
      `MultiBackend` class.

    - Generalized `Transformable` instances for pairs and tuples

		Previously, the components of the tuples were required to have
		the same type; but everything still works as long as they all
		share the same vector space.  This is actually useful in
		practice: say, if we wanted to pair a diagram with a path and
		then apply the same transformation to both.

* **Improvements**

    - More efficient implementation of `diameter`

* **Dependency/version changes**

    - Tested with GHC 7.6.1
    - allow `base-4.6`
    - allow `containers-0.5.*`
    - allow `MemoTrie-0.6.1`

* **Bug fixes**

    - juxtaposeDefault now correctly handles empty envelopes (#37)

        `juxtaposeDefault` is now the identity on the second object if
        either one has an empty envelope.  In particular this means that
        `mempty` is now an identity element for `beside` and friends.

0.5.0.1: 11 May 2012
--------------------

* Update `MemoTrie` upper bound to allow `MemoTrie-0.5`

0.5: 9 March 2012
-----------------

* New features:
    - New `Juxtaposable` class
    - New `NullBackend` and `D` types, for conveniently giving a
      monomorphic type to diagrams when we don't care which one it is.
    - [\#27](http://code.google.com/p/diagrams/issues/detail?id=27): Change type of `adjustDia` to return a new options record
      (with an explicitly filled-in size)

* New instances:
    - `Enveloped`, `HasOrigin`, `Juxtaposable`, `HasStyle`, and `Transformable`
      instances for `Set`s and tuples
    - `V Double = Double`
    - `Juxtaposable` and `Boundable` instances for `Map`

* API changes
    - `AnnDiagram` renamed to `QDiagram`
    - [\#61](http://code.google.com/p/diagrams/issues/detail?id=61): terminology change from "bounds" to "envelope"
        + `boundary` -> `envelopeP`
        + "bounding region" -> "envelope"
        + `Bounds` -> `Envelope`
        + `Boundable` -> `Enveloped`
        + `getBounds` -> `getEnvelope`
        + *etc.*
    - Split out definition of `Point` into separate package
      ([`vector-space-points`](http://hackage.haskell.org/package/vector%2Dspace%2Dpoints))
    - The `Point` constructor `P` is no longer exported from
      `Graphics.Rendering.Diagrams`.  See the `Diagrams.TwoD.Types` module
      from `diagrams-lib` for new tools for working with abstract 2D
      points.  If you really need the `P` constructor, import
      `Graphics.Rendering.Diagrams.Points`.
    - Name-related functions now return "located bounding functions"
      instead of pairs of points and bounds, to allow for future
      expansion.

* Dependency/version changes:
    - `vector-space` 0.8 is now required.
    - Bump base upper bound to allow 4.5; now tested with GHC 7.4.1.

* Bug fixes:
    - Bug fix related to empty envelopes

0.4: 23 October 2011
--------------------

* improved documentation
* a few new instances (Newtype Point, Boundable Point)
* new functions (value, clearValue, resetValue) for working with
  alternate query monoids

0.3: 18 June 2011
-----------------

* big overhaul of name maps:
    - allow arbitrary types as atomic names
    - carry along bounding functions as well as names in NameMaps
    - additional functions for querying information associated with names
* fix for issue #34 (fix behavior of setBounds)
* Transformable and HasOrigin instances for Transformations

0.2: 3 June 2011
----------------

* bounding regions can now be overridden
* new namePoint function for more flexibly assigning names to arbitrary points
* add HasStyle, Boundable, and HasOrigin instances for lists
* add a "trivial backend"
* transformable attributes

0.1.1: 18 May 2011
------------------

* link to new website

0.1: 17 May 2011
----------------

* initial preview release
