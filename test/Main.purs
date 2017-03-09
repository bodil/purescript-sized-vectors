module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Typelevel.Num (toInt, d6, d3, d9, d2)
import Data.Vec (slice, tail, drop, take, lengthT, concat, replicate, (+>), empty)
import Prelude (($), Unit, bind)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  suite "vec" do
    let vec1 = replicate d2 1
        vec2 = replicate d3 2
        vec3 = replicate d9 3
    test "cons length" do
      equal 3 $ toInt $ lengthT $ 1 +> 2 +> 3 +> empty
    test "replicate length" do
      equal 2 $ toInt $ lengthT vec1
    test "concat length" do
      equal 5 $ toInt $ lengthT (concat vec1 vec2)
    test "take length" do
      equal 2 $ toInt $ lengthT (take d2 vec2)
    test "drop length" do
      equal 1 $ toInt $ lengthT (drop d2 vec2)
    test "tail length" do
      equal 1 $ toInt $ lengthT (tail vec1)
    test "slice length" do
      equal 3 $ toInt $ lengthT (slice d3 d6 vec3)
