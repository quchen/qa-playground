QA playground
=============

This example project demonstrates basic usages of many important QA tools
available in the Haskell ecosystem.

- [x] Test framework **Tasty**
    - [x] Probabilistic batch testing with **QuickCheck**
    - [ ] Deterministic batch testing with **Smallcheck**
    - [x] Example test case testing with **HUnit**
- [x] Generating coverage reports with **HPC**
- [x] Benchmarking with **Criterion**

Execute it with

```bash
stack build --test --coverage --benchmark
```
