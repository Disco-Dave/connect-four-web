import Spec (spec)
import Test.Hspec (hspec, parallel)

main :: IO ()
main = hspec . parallel $ spec
