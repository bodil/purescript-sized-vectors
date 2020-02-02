# purescript-sized-vectors

[![Build Status](https://travis-ci.org/bodil/purescript-sized-vectors.svg?branch=master)](https://travis-ci.org/bodil/purescript-sized-vectors)

Idris style sized vectors using [purescript-typelevel](https://github.com/bodil/purescript-typelevel).

- [API docs on Pursuit](http://pursuit.purescript.org/packages/purescript-sized-vectors/)

## Installation

```bash
bower install --save purescript-sized-vectors
```

or

```
spago install sized-vectors
```

## Usage

The sized vector datatype `(Nat s) ⇒ Vec s a` is implemented as a newtype wrapper around an `Array`, but has a known size `s` at the type level (which must be a type in `Data.Typelevel.Num.Nat`). Thus, the type checker is able to catch things like index out of bounds errors instead of forcing you to deal with `Maybe` values. Of course, this comes with its own set of limitations: operations where the size of a resulting vector can't be determined from the input types are impossible, an example being `filter`. However, you still have the full `Functor` - `Applicative` - `Monad` suite available, in addition to the `Foldable` and `Traversable` classes, and the full set of equivalents to `Array` operations allowed by `Vec`'s constraints. Note however that `apply`, `pure`, and `bind` do not work in the same way as they do with normal `Array`s, but instead make sure to keep the `Vec` lengths the same, leading to fully law-abiding instances.

You can construct `Vec`s in only two ways: through consing `1 +> 2 +> 3 +> empty` or through factory functions `singleton "a Vec of one single string"` and `replicate d5 "this string is repeated 5 times"`. Note the `d5` in the last example: `Vec` operations always take type level numbers where `Array` operations would take integer values.

## Releases

See [Changelog](CHANGELOG.md).

## LICENCE

Copyright 2016 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.
