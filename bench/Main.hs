import Criterion.Main
import FindFast (findFast)

main =
  defaultMain
    [ bgroup
        "findFast"
        [bench "find grep" $ nfIO (findFast "grep" ".")]
    ]
