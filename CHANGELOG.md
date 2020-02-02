# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Start using a Changelog.

## [5.0.0] - 2020-01-30

### Added

- New instances
  - `FunctorWithIndex`
  - `TraversableWithIndex`
  - `Traversable`
  - `EncodeJson`
  - `DecodeJson`
  - `Arbitrary`
  - `Coarbitrary`
  - `Generic`
  - `CommutativeRing`
  - `Monad`
  - `Bind`
  - `Semigroup`
  - `Monoid`
- CI checks if code is formatted with `purty`.
- Start using `quickcheck-laws` when possible in tests.

### Changed

- `range` was adjusted to work similar to `range` known from `Unfoldable`: Treat both numeric arguments as inclusive endpoints. Resulting sequence may be ascending or descending.
- `range` uses typelevel naturals for both endpoints.
- `Nat` constraints are only kept in functions and classes that absolutely need them.
- `Show` instance is more specific now: `(x1 +> x2 +> ...xN +> empty)`. Rather than just the `Array` one of `[x1, x2, ...xN]`.

### Removed

- `range'` was dropped as it was inconsistent with `range` known from `Unfoldable`.

### Fixed

- Version format
- `Apply` instance is now implemented as "zip" instead of "cartesian product". This was a bug as the latter changes the size of the result to `n * n`.

## [4.0.0] - 2019-10-15 (not published on pursuit)

### Added

- New instances
  - `Semiring`
  - `Ring`
