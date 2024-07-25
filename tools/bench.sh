set -e

export LANG="C.UTF-8"

cabal bench --project-file=cabal.arithmoi.project --benchmark-options \
  '--csv benchmarks/bench.csv --svg benchmarks/bench.svg'
