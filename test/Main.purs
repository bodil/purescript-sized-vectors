module Test.Main where

import Prelude
import Data.Distributive (distribute)
import Effect (Effect)
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
import Data.Typelevel.Num (D1, D2, D3, D4, D9, d2, d3, d6, toInt)
import Data.Vec (Vec, concat, dotProduct, drop, drop', empty, fill, fromArray, length, lengthT, range, range', replicate, replicate', slice, slice', tail, take, take', (+>))
import Data.Vec as Vec
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, discard, pure, ($), (+))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "vec" do
    let vec1 = replicate d2 1
        vec2 = replicate d3 2
        (vec3 :: Vec D9 Int) = replicate' 3
        (vec4 :: Vec D4 Int) = unsafePartial $ fromJust $ fromArray [1, 2, 3, 4]
    test "fill" do
      equal (Vec.vec3 1 2 3) $ fill (_ + 1)
    test "range'" do
      equal (Vec.vec3 4 5 6) $ range' 4
      equal (Vec.vec2 4 5) $ range' 4
    test "range" do
      equal (Vec.vec3 4 5 6) $ range 4 d3
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
      equal 2 $ toInt $ lengthT (take' vec2) :: D2
    test "drop length" do
      equal 1 $ length $ drop d2 vec2
      equal 1 $ toInt $ lengthT (drop' vec2) :: D1
    test "tail length" do
      equal 1 $ toInt $ lengthT (tail vec1)
    test "slice length" do
      equal 3 $ length $ slice d3 d6 vec3
      equal 3 $ toInt $ lengthT (slice' d3 vec3) :: D3
    test "pure replicates" do
      let vec3' = pure 3 :: Vec D9 Int
      equal vec3 vec3'
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
      let v1 = Vec.vec3 1 2 3
          v2 = Vec.vec3 4 5 6
          v3 = Vec.vec3 7 8 9
      equal ((v1 + v2) + v3) (v1 + (v2 + v3))
      equal (zero + v1) (v1 + zero)
      equal (v1 + v2) (v2 + v1)
      equal ((v1 * v2) * v3) (v1 * (v2 * v3))
      equal (one * v1) (v1 * one)
      equal (v1 * (v2 + v3)) ((v1 * v2) + (v1 * v3))
      equal ((v1 + v2) * v3) ((v1 * v3) + (v2 * v3))
      equal (zero * v1) zero
      equal (v1 * zero) zero
    test "ring" do
      let v1 = Vec.vec3 1 2 3
      equal (v1 - v1) zero
      equal ((zero - v1) + v1) zero
    test "dotProduct" do
      equal 0 $ dotProduct (Vec.vec3 1 0 0) (Vec.vec3 0 1 0)
      equal 32 $ dotProduct (Vec.vec3 1 2 3) (Vec.vec3 4 5 6)
