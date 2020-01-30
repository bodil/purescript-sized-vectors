module Test.Main where

import Prelude

import Data.Const (Const(..))
import Data.Distributive (distribute)
import Data.Foldable (foldMap, foldl, foldr)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Typelevel.Num (D1, D2, D3, D4, D9, d2, d3, d4, d6, toInt)
import Data.Vec (Vec, concat, dotProduct, drop, drop', empty, fill, fromArray, length, lengthT, range, replicate, replicate', slice, slice', tail, take, take', (+>))
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Laws (A, B)
import Test.QuickCheck.Laws.Control (checkApply, checkApplicative, checkBind, checkMonad)
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkFoldable, checkFoldableFunctor, checkFunctor, checkMonoid, checkRing, checkSemigroup, checkSemiring)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..), Proxy2(..))


main :: Effect Unit
main = runTest do
  suite "vec" do
    let vec1 = replicate d2 1
        vec2 = replicate d3 2
        (vec3 :: Vec D9 Int) = replicate' 3
        (vec4 :: Vec D4 Int) = unsafePartial $ fromJust $ fromArray [1, 2, 3, 4]
    test "fill" do
      equal (Vec.vec3 1 2 3) $ fill (_ + 1)
    test "range" do
      equal (Vec.vec3 4 5 6) $ range d4 d6
      equal (Vec.vec3 6 5 4) $ range d6 d4
    test "cons length" do
      equal 3 $ toInt $ lengthT $ 1 +> 2 +> 3 +> empty
      equal 3 $ length $ 1 +> 2 +> 3 +> empty
    test "replicate length" do
      equal 2 $ toInt $ lengthT vec1
      equal 9 $ length vec3
    test "fromArray length" do
      equal 4 $ toInt $ lengthT vec4
      equal 4 $ length vec4
    test "concat length" do
      equal 5 $ toInt $ lengthT (concat vec1 vec2)
    test "take length" do
      equal 2 $ length $ take d2 vec2
      equal 2 $ toInt $ (lengthT (take' vec2) :: D2)
    test "drop length" do
      equal 1 $ length $ drop d2 vec2
      equal 1 $ toInt $ (lengthT (drop' vec2) :: D1)
    test "tail length" do
      equal 1 $ toInt $ lengthT (tail vec1)
    test "slice length" do
      equal 3 $ length $ slice d3 d6 vec3
      equal 3 $ toInt $ (lengthT (slice' d3 vec3) :: D3)
    test "apply law: Associative composition" do
      liftEffect $ checkApply (Proxy2 :: Proxy2 (Vec D9))
    test "applicative law: Identity, Composition, Homomorphism, Interchange" do
      liftEffect $ checkApplicative (Proxy2 :: Proxy2 (Vec D9))
    test "bind law: Associativity" do
      liftEffect $ checkBind (Proxy2 :: Proxy2 (Vec D9))
    test "monad law: Left Identity, Right Identity" do
      liftEffect $ checkMonad (Proxy2 :: Proxy2 (Vec D9))
    test "pure replicates" do
      let vec3' = pure 3 :: Vec D9 Int
      equal vec3 vec3'
    test "foldable" do
      liftEffect $ checkFoldable (Proxy2 :: Proxy2 (Vec D9))
      liftEffect $ checkFoldableFunctor (Proxy2 :: Proxy2 (Vec D9))
    test "traversable 1" do
      let vecOfArrays = [1] +> [2] +> [3] +> empty
          expected = 1 +> 2 +> 3 +> empty
      equal [expected] $ sequence vecOfArrays
    test "traversable 2" do
      let vecOfArrays = [1,2] +> [2,3] +> empty
          expected = [ 1 +> 2 +> empty
                     , 1 +> 3 +> empty
                     , 2 +> 2 +> empty
                     , 2 +> 3 +> empty
                     ]
      equal expected $ sequence vecOfArrays
    test "distributive" do
      let vecs1 = (1 +> 2 +> empty)
               +> (3 +> 4 +> empty)
               +> (5 +> 6 +> empty)
               +> empty

          vecs2 =  (1 +> 3 +> 5 +> empty)
                +> (2 +> 4 +> 6 +> empty)
                +> empty
      equal vecs2 $ distribute vecs1
      equal vecs1 $ distribute vecs2
      equal vecs1 $ distribute $ distribute vecs1
      equal vecs2 $ distribute $ distribute vecs2
    test "semiring" do
      liftEffect $ checkSemiring (Proxy :: Proxy (Vec D9 Int))
    test "ring" do
      liftEffect $ checkRing (Proxy :: Proxy (Vec D9 Int))
    test "commutative ring" do
      liftEffect $ checkCommutativeRing (Proxy :: Proxy (Vec D9 Int))
    test "semigroup" do
      liftEffect $ checkSemigroup (Proxy :: Proxy (Vec D9 String))
    test "monoid" do
      liftEffect $ checkMonoid (Proxy :: Proxy (Vec D9 String))
    test "dotProduct" do
      equal 0 $ dotProduct (Vec.vec3 1 0 0) (Vec.vec3 0 1 0)
      equal 32 $ dotProduct (Vec.vec3 1 2 3) (Vec.vec3 4 5 6)
    suite "functorWithIndex" do
      test "identity law" do
        quickCheck \(vec :: Vec D9 A) ->
          mapWithIndex (\_ a -> a) vec == identity vec
      test "composition law" do
        quickCheck \(f :: Int -> B -> A) (g :: Int -> A -> B) (vec :: Vec D9 A) -> 
          (mapWithIndex f <<< mapWithIndex g) vec == mapWithIndex (\i -> f i <<< g i) vec
    test "functor law: identity, composition" do
      liftEffect $ checkFunctor (Proxy2 :: Proxy2 (Vec D9))
    suite "foldableWithIndex" do
      test "foldr compatible" do
        quickCheck \(f :: A -> B -> B) (b :: B) (fa :: Vec D9 A) ->
          foldr f b fa == foldrWithIndex (const f) b fa
      test "foldl compatible" do
        quickCheck \(f :: B -> A -> B) (b :: B) (fa :: Vec D9 A) ->
          foldl f b fa == foldlWithIndex (const f) b fa
      test "foldMapWithIndex compatible" do
        quickCheck \(f :: A -> B) (fa :: Vec D9 A) ->
          foldMap f fa == foldMapWithIndex (const f) fa
    suite "traversableWithIndex" do
      test "compatible with traversable" do
        quickCheck \(f :: A -> Identity B) (fa :: Vec D9 A) ->
          traverse f fa == traverseWithIndex (const f) fa
      test "compatible with foldableWithIndex" do
        quickCheck \(f :: Int -> A -> Identity B) (fa :: Vec D9 A) ->
          foldMapWithIndex f fa == (unwrap <<< traverseWithIndex (\i -> Const <<< f i)) fa
      test "compatible with functorWithIndex" do
        quickCheck \(f :: Int -> A -> Vec D9 B) (fa :: Vec D9 A) ->
          mapWithIndex f fa == (unwrap <<< traverseWithIndex (\i -> Identity <<< f i)) fa 
