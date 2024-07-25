set -e

export LANG="C.UTF-8"

cabal build prime-prof --project-file=cabal.prime-prof.project

cabal run prime-prof --project-file=cabal.prime-prof.project -- +RTS -p -RTS
