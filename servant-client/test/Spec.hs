import           Servant.ClientSpec (failSpec, spec)

main :: IO ()
main = do
  spec
  failSpec

