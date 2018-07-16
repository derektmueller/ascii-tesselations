import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "tesselate" $ do
    it "tesselates" $ do
      tesselate2x2 Ninety ["#"] `shouldBe` ["##", "##"]
