To see the problem:

- enter the nix environment with `nix develop .#`
- go to `./clash` and try `cabal test` to see the haskell tests run (and pass)
- go to `./clash/sim` and run `python test_top.py` to see the simulator test run. Technically it passes but that's because there's no real test, if you look at the log output you can see the problem.

then to make it pass:
- remove `axiPassthrough` from `ethAxisCircuit` and re-run the simulator test
