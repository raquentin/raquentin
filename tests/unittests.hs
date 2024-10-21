import Test.Tasty
import qualified Tests.Amplifiers.Gain
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Amplifiers.Gain.tests
      ]
