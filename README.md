# purescript-sized-vectors

Idris style sized vectors using [purescript-typelevel](https://github.com/bodil/purescript-typelevel).

* [API docs on Pursuit](http://pursuit.purescript.org/packages/purescript-sized-vectors/)

## Usage

The sized vector datatype `(Nat s) ⇒ Vec s a` is implemented as a newtype wrapper around an `Array`, but has a known size `s` at the type level (which must be a type in `Data.Typelevel.Num.Nat`). Thus, the type checker is able to catch things like index out of bounds errors instead of forcing you to deal with `Maybe` values. Of course, this comes with its own set of limitations: operations where the size of a resulting vector can't be determined from the input types are impossible, including classics like `bind` and `filter`. However, you do at least have `Functor`, `Applicative` and `Foldable` implementations available, in addition to the full set of equivalents to `Array` operations allowed by `Vec`'s constraints.

You can construct `Vec`s in only two ways: through consing `1 +> 2 +> 3 +> empty` or through factory functions `singleton "a Vec of one single string"` and `replicate d5 "this string is repeated 5 times"`. Note the `d5` in the last example: `Vec` operations always take type level numbers where `Array` operations would take integer values.

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
