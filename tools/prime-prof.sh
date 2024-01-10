set -e

export LANG="C.UTF-8"

cabal build prime-prof --project-file=cabal.project.prime-prof

cabal run prime-prof --project-file=cabal.project.prime-prof -- +RTS -p -RTS