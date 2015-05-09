import Servant.ClientSpec (spec, failSpec)

main :: IO ()
main = do
  spec
  failSpec

