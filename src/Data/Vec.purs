module Data.Vec
  ( Vec
  , empty
  , cons
  , (+>)
  , snoc
  , uncons
  , singleton
  , vec2
  , vec3
  , fill
  , range
  , replicate
  , replicate'
  , fromArray
  , length
  , lengthT
  , toArray
  , toUnfoldable
  , index
  , (!!)
  , index'
  , concat
  , updateAt
  , modifyAt
  , insertAt
  , deleteAt
  , head
  , last
  , tail
  , init
  , insert
  , insertBy
  , slice
  , slice'
  , take
  , take'
  , drop
  , drop'
  , zip
  , zipWith
  , zipWithE
  , unzip
  , sort
  , sortBy
  , reverse
  , dotProduct
  ) where

import Prelude
  ( class Eq
  , (==)
  , class Semiring
  , class Ring
  , class CommutativeRing
  , class Semigroup
  , class Monoid
  , (*)
  , pure
  , append
  , sub
  , add
  , mempty
  , zero
  , class Show
  , map
  , class Functor
  , class Apply
  , class Applicative
  , class Bind
  , class Monad
  , mul
  , (<>)
  , (-)
  , (<*>)
  , one
  , show
  , (<$>)
  , flap
  , (<<<)
  , Ordering
  , ($)
  , class Ord
  , identity
  , const
  )
import Control.Apply (lift2)
import Data.Array as Array
import Data.Array.Partial as ArrayP
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.Foldable (foldl, foldMap, class Foldable, sum)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence, class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Num (class Min, class Max, class Sub, class LtEq, class Pred, class Lt)
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (D0, D1, D2, D3)
import Data.Typelevel.Num.Sets (class Nat, class Pos, toInt, toInt')
import Data.Typelevel.Undefined (undefined)
import Data.Unfoldable (class Unfoldable)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic.Rep (class Generic)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Type.Proxy (Proxy(..))

-- | `Vec s a` is an array with a fixed size `s` defined at the type level.
newtype Vec s a
  = Vec (Array a)

-- | An empty vector.
empty :: forall a. Vec D0 a
empty = Vec []

-- | Prepend a value to the front of a vector, creating a vector of size `Succ s`.
cons :: forall s s' a. Succ s s' => a -> Vec s a -> Vec s' a
cons x (Vec xs) = Vec (Array.cons x xs)

infixr 5 cons as +>

-- | Append a value to the end of a vector, creating a vector of size `Succ s`.
snoc :: forall s s' a. Succ s s' => a -> Vec s a -> Vec s' a
snoc x (Vec xs) = Vec (Array.snoc xs x)

-- | Get the head and the tail of a non-empty vector.
uncons :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> { head :: a, tail :: Vec s2 a }
uncons (Vec v) = unsafePartial { head: ArrayP.head v, tail: Vec (ArrayP.tail v) }

-- | Construct a vector containing only a single element.
singleton :: forall a. a -> Vec D1 a
singleton x = x +> empty

-- | shortcut for creating a 2d-`Vec`
vec2 :: forall a. a -> a -> Vec D2 a
vec2 x y = x +> y +> empty

-- | shortcut for creating a 3d-`Vec`
vec3 :: forall a. a -> a -> a -> Vec D3 a
vec3 x y z = x +> y +> z +> empty

-- | fill vec using a function which is given indices
fill :: forall a s. Nat s => (Int -> a) -> Vec s a
fill f = Vec (map f range_)
  where
  s = toInt' (Proxy :: Proxy s)

  range_ = case s of
    0 -> []
    otherwise -> (0 `Array.range` (s - 1))

-- | Construct a vector of a given length containing the same element repeated.
replicate :: forall s a. Nat s => s -> a -> Vec s a
replicate = const replicate'

replicate' :: forall s a. Nat s => a -> Vec s a
replicate' a = Vec (Array.replicate (toInt' (Proxy :: Proxy s)) a)

range ::
  forall n1 n2 n3 max min diff.
  Nat n1 =>
  Nat n2 =>
  Max n1 n2 max =>
  Min n1 n2 min =>
  Sub max min diff =>
  Succ diff n3 =>
  n1 -> n2 -> Vec n3 Int
range a b = Vec (Array.range (toInt a) (toInt b))

-- | Convert an array to a vector.
fromArray :: forall s a. Nat s => Array a -> Maybe (Vec s a)
fromArray xs =
  if Array.length xs == toInt' (Proxy :: Proxy s) then
    Just (Vec xs)
  else
    Nothing

-- | Get the length of a vector as an integer.
length :: forall s a. Nat s => Vec s a -> Int
length _ = toInt' (Proxy :: Proxy s)

-- | Get the length of a vector as a type level number.
lengthT :: forall s a. Nat s => Vec s a -> s
lengthT _ = undefined

-- | Convert a vector into an array. This simply unwraps the underlying array, so it has no runtime cost.
toArray :: forall s a. Vec s a -> Array a
toArray (Vec xs) = xs

-- | Convert a vector into any `Unfoldable`.
toUnfoldable :: forall f s a. Unfoldable f => Nat s => Vec s a -> f a
toUnfoldable (Vec v) = Array.toUnfoldable v

-- | Get the element at a given index inside a vector. Index out of bounds errors
-- | are caught at compile time.
-- |
-- | Example:
-- |
-- |     myVector = 1 +> 2 +> 3 +> 4 +> empty
-- |     value = index myVector d2
-- |     -- value == 3
-- |     value = index myVector d4
-- |     -- out of bounds so does not type check
index :: forall i s a. Nat i => Lt i s => Vec s a -> i -> a
index (Vec xs) i = unsafePartial (Array.unsafeIndex xs (toInt i))

infixl 8 index as !!

-- | Value-level indexation with runtime bounds check.
index' :: forall s a. Vec s a -> Int -> Maybe a
index' (Vec xs) = Array.index xs

-- | Concatenate two vectors together.
concat :: forall s1 s2 s3 a. Add s1 s2 s3 => Vec s1 a -> Vec s2 a -> Vec s3 a
concat (Vec xs1) (Vec xs2) = Vec (Array.concat [ xs1, xs2 ])

-- | Update a vector with a given value inserted at a given index.
updateAt :: forall i s a. Nat i => Lt i s => i -> a -> Vec s a -> Vec s a
updateAt i v (Vec xs) = Vec (unsafePartial (fromJust (Array.updateAt (toInt i) v xs)))

-- | Update a vector at a given index using a function.
modifyAt :: forall i s a. Nat i => Lt i s => i -> (a -> a) -> Vec s a -> Vec s a
modifyAt i f (Vec xs) = Vec (unsafePartial (fromJust (Array.modifyAt (toInt i) f xs)))

-- | Insert a value at a given index inside a vector, returning a vector
-- | that is one element larger.
insertAt :: forall i s1 s2 a. Nat i => Lt i s1 => Succ s1 s2 => i -> a -> Vec s1 a -> Vec s2 a
insertAt i a (Vec xs) = Vec (unsafePartial (fromJust (Array.insertAt (toInt i) a xs)))

-- | Remove an element at a given index inside a vector, returning a vector
-- | that is one element smaller.
deleteAt :: forall i s1 s2 a. Nat i => Lt i s1 => Pred s1 s2 => i -> Vec s1 a -> Vec s2 a
deleteAt i (Vec xs) = Vec (unsafePartial (fromJust (Array.deleteAt (toInt i) xs)))

-- | Get the head of a non-empty vector.
head :: forall s a. Pos s => Vec s a -> a
head (Vec xs) = unsafePartial (ArrayP.head xs)

-- | Get the last element of a non-empty vector.
last :: forall s a. Pos s => Vec s a -> a
last (Vec xs) = unsafePartial (ArrayP.last xs)

-- | Get the tail of a non-empty vector.
tail :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> Vec s2 a
tail (Vec xs) = Vec (unsafePartial (ArrayP.tail xs))

-- | Get all but the last element of a non-empty vector.
init :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> Vec s2 a
init (Vec xs) = Vec (unsafePartial (ArrayP.init xs))

-- | Insert an element into a sorted vector.
insert :: forall s1 s2 a. Succ s1 s2 => Ord a => a -> Vec s1 a -> Vec s2 a
insert a (Vec v) = Vec (Array.insert a v)

-- | Insert an element into a sorted vector using an ordering function.
insertBy :: forall s1 s2 a. Succ s1 s2 => (a -> a -> Ordering) -> a -> Vec s1 a -> Vec s2 a
insertBy f a (Vec v) = Vec (Array.insertBy f a v)

-- | Get a sub-vector from index `i1` up to but not including index `i2`.
slice :: forall i1 i2 s1 s2 a. Nat i1 => Nat i2 => LtEq i1 s1 => LtEq i2 s1 => LtEq i1 i2 => Sub i2 i1 s2 => i1 -> i2 -> Vec s1 a -> Vec s2 a
slice i1 i2 (Vec xs) = Vec (Array.slice (toInt i1) (toInt i2) xs)

slice' :: forall i1 i2 s1 s2 a. Nat i1 => Nat i2 => LtEq i1 s1 => LtEq i2 s1 => LtEq i1 i2 => Sub i2 i1 s2 => i1 -> Vec s1 a -> Vec s2 a
slice' i1 (Vec xs) = Vec (Array.slice (toInt i1) (toInt' (Proxy :: Proxy i2)) xs)

-- | Get the first `c` elements from a vector.
take :: forall c s a. Nat c => LtEq c s => c -> Vec s a -> Vec c a
take = const take'

take' :: forall c s a. Nat c => LtEq c s => Vec s a -> Vec c a
take' (Vec xs) = Vec (Array.take (toInt' (Proxy :: Proxy c)) xs)

-- | Drop the first `c` elements from a vector.
drop :: forall c s1 s2 a. Nat c => LtEq c s1 => Sub s1 c s2 => c -> Vec s1 a -> Vec s2 a
drop c (Vec xs) = Vec (Array.drop (toInt c) xs)

-- the typchecker doesn't like this:
-- drop = const drop'
drop' :: forall c s1 s2 a. Nat c => LtEq c s1 => Sub s1 c s2 => Vec s1 a -> Vec s2 a
drop' (Vec xs) = Vec (Array.drop (toInt' (Proxy :: Proxy c)) xs)

-- | Zip two vectors together into a vector of tuples.
-- |
-- | The new vector will be the size of the smallest input vector, and
-- | superfluous elements from the other will be discarded.
zip :: forall s1 s2 s3 a b. Min s1 s2 s3 => Vec s1 a -> Vec s2 b -> Vec s3 (Tuple a b)
zip (Vec v1) (Vec v2) = Vec (Array.zip v1 v2)

-- | Zip two vectors together using a combining function.
-- |
-- | The new vector will be the size of the smallest input vector, and
-- | superfluous elements from the other will be discarded.
zipWith :: forall s1 s2 s3 a b c. Min s1 s2 s3 => (a -> b -> c) -> Vec s1 a -> Vec s2 b -> Vec s3 c
zipWith f (Vec v1) (Vec v2) = Vec (Array.zipWith f v1 v2)

-- | Zip two vectors with equal length together using a combining function.
zipWithE :: forall s a b c. (a -> b -> c) -> Vec s a -> Vec s b -> Vec s c
zipWithE f (Vec v1) (Vec v2) = Vec (Array.zipWith f v1 v2)

-- | Unzip a vector of tuples into a tuple of vectors.
unzip :: forall s a b. Vec s (Tuple a b) -> Tuple (Vec s a) (Vec s b)
unzip (Vec v) = case Array.unzip v of
  (Tuple v1 v2) -> Tuple (Vec v1) (Vec v2)

-- | Sort a vector of `Ord`s.
sort :: forall s a. Ord a => Vec s a -> Vec s a
sort (Vec v) = Vec (Array.sort v)

-- | Sort a vector using an ordering function.
sortBy :: forall s a. (a -> a -> Ordering) -> Vec s a -> Vec s a
sortBy f (Vec v) = Vec (Array.sortBy f v)

-- | Reverse a vector.
reverse :: forall s a. Vec s a -> Vec s a
reverse (Vec v) = Vec (Array.reverse v)

derive instance genericVec :: Generic a a' => Generic (Vec s a) _

derive newtype instance eqVec :: Eq a => Eq (Vec s a)

derive newtype instance ordVec :: Ord a => Ord (Vec s a)

derive newtype instance functorVec :: Functor (Vec s)

derive newtype instance foldableVec :: Foldable (Vec s)

derive newtype instance functorWithIndexVec :: FunctorWithIndex Int (Vec s)

derive newtype instance foldableWithIndexVec :: FoldableWithIndex Int (Vec s)

derive newtype instance traversableWithIndexVec :: TraversableWithIndex Int (Vec s)

derive newtype instance traversableVec :: Traversable (Vec s)

derive newtype instance encodeJsonVec :: EncodeJson a => EncodeJson (Vec s a)

derive newtype instance decodeJsonVec :: DecodeJson a => DecodeJson (Vec s a)

instance arbitraryVec :: (Arbitrary a, Nat s) => Arbitrary (Vec s a) where
  arbitrary = Vec <$> (sequence (Array.replicate s arbitrary))
    where
    s = toInt (undefined :: s)

instance coarbitraryVec :: (Coarbitrary a, Nat s) => Coarbitrary (Vec s a) where
  coarbitrary = foldl (\f x -> f <<< coarbitrary x) identity

instance applyVec :: Apply (Vec s) where
  apply a b = zipWithE ($) a b

instance applicativeVec :: Nat s => Applicative (Vec s) where
  pure a = replicate' a

instance bindVec :: Nat s => Bind (Vec s) where
  bind vec f = distribute f <*> vec

instance monadVec :: Nat s => Monad (Vec s)

instance distributiveVec :: Nat s => Distributive (Vec s) where
  collect = collectDefault
  distribute vs =
    let
      as = map toArray vs

      len = toInt' (Proxy :: Proxy s)

      indexes = if len == 0 then [] else Array.range 0 (len - 1)
    in
      Vec (flap (unsafePartial Array.unsafeIndex <$> as) <$> indexes)

instance showVec :: (Nat s, Show a) => Show (Vec s a) where
  show v = "(" <> foldMap (\e -> show e <> " +> ") v <> "empty)"

instance semiringVec :: (Semiring a, Nat s) => Semiring (Vec s a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance ringVec :: (Ring a, Nat s) => Ring (Vec s a) where
  sub = lift2 sub

instance commutativeRingVec :: (CommutativeRing a, Nat s) => CommutativeRing (Vec s a)

instance semigroupVec :: (Semigroup a, Nat s) => Semigroup (Vec s a) where
  append = lift2 append

instance monoidVec :: (Monoid a, Nat s) => Monoid (Vec s a) where
  mempty = pure mempty

dotProduct :: forall s a. Semiring a => Vec s a -> Vec s a -> a
dotProduct a b = sum (zipWithE (*) a b)
