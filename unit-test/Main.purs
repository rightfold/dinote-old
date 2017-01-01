module Test.Main
( main
) where

import NN.Prelude
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main = runTest do
    test "logic" do
        Assert.assert "truth" true
        Assert.assert "not falsehood" (not false)
