module Hexmap.Coordinates.ConversionSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Lens
import           Control.Monad                 (forM_)
import           Hexmap.Coordinates.Axial
import           Hexmap.Coordinates.Conversion
import           Hexmap.Coordinates.Offset
import           Linear.V2

main :: IO ()
main = hspec spec

prop_Iso anIso = property $ \a -> over anIso id a == a

-- LOL
(.:) = (.) . (.)

instance Arbitrary a => Arbitrary (Offset a) where
  arbitrary = Offset .: V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Axial a) where
  arbitrary = Axial .: V2 <$> arbitrary <*> arbitrary

axial x y = Axial (V2 x y)
offset x y = Offset (V2 x y)

conversions1 :: [(Axial Int, Offset Int)]
conversions1 =
  [ (axial 0 0, offset 0 0)
  , (axial 0 1, offset 0 1)
  , (axial 0 2, offset 1 2)
  , (axial 0 3, offset 1 3)
  ]


spec :: Spec
spec = do
  describe "conversion between" $ do
    context "Axial and Offset" $ do
      it "is an isomorphism (forwards)" $ do
        prop_Iso (convert :: Iso' (Axial Int) (Offset Int))
      it "is an isomorphism (backwards)" $ do
        prop_Iso (convert :: Iso' (Offset Int) (Axial Int))
      it "converts specific cases" $
        forM_ conversions1 $ \(a, b) -> do
          a ^. convert `shouldBe` b
          b ^. convert `shouldBe` a
