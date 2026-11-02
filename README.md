# PubGrub

[![GitHub Actions](https://img.shields.io/github/actions/workflow/status/brandonchinn178/pubgrub-hs/ci.yml?branch=main)](https://github.com/brandonchinn178/pubgrub-hs/actions?query=branch%3Amain)
[![](https://img.shields.io/hackage/v/pubgrub-hs)](https://hackage.haskell.org/package/pubgrub-hs)

PubGrub is a high performance version resolution algorithm with good error messages, it finds a set of packages and versions that satisfy all the constraints of a set of packages and their transitive dependencies. This algorithm is used by [dart](https://medium.com/@nex3/pubgrub-2fb6470504f), and the [rust](https://github.com/pubgrub-rs/pubgrub) implementation is used by uv and eventually cargo.

References:
* https://nex3.medium.com/pubgrub-2fb6470504f
* https://deepwiki.com/pubgrub-rs/pubgrub/3.2-the-pubgrub-algorithm
