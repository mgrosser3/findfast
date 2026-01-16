import Criterion.Main
import FindFast (findFast, findFastRecursive)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "findFast"
        [bench "find grep" $ nfIO (findFast "grep" ".")],
      bgroup
        "findFastRecursive"
        [bench "find grep" $ nfIO (findFastRecursive "grep" ".")]
    ]
