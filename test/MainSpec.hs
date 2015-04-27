
module MainSpec where

import           Prelude        ()
import           Prelude.Compat

import           Control.Monad
import           Test.Hspec

import           Run
import           Test.Utils

spec :: Spec
spec = do
  describe "constant" $ do
    it "works" $ do
      compareToList
        (constant 0.4)
        1
        ["0.400000"]

  describe "rect" $ do
    it "works" $
      compareToList
        (constant 0.3 >>= rect)
        5
        ["-1.000000", "-1.000000", "1.000000", "1.000000", "-1.000000"]

  describe "volume" $ do
    it "works like multiplying with a constant" $
      (volume 0.3 =<< saw 0.4)
        ===
      (do
        signal <- saw 0.4
        join (mult <$> constant 0.3 <*> pure signal))
