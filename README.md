GLL Haskell Parser
==================

I couldn't find a Github repo for the [Haskell GLL Parser], so I created one.
This is for experiments in using this parser for the [K Framework].

[Haskell GLL Parser]: <https://hackage.haskell.org/package/gll>
[K Framework]: <https://github.com/kframework>

### Testing Setup

To run the tests, I am currently doing:

```sh
cabal configure --enable-tests --disable-shared
cabal build
cabal test
```

Ideally we would get this working with `stack` instead.
