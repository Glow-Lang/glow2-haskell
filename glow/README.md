WIP Experimental Haskell rewrite of parts of glow.

To build:

```sh
git submodule init
git submodule update
cabal build all
```

# Notes

Right now we're vendoring the `sexpr-parser` package as a submodule,
so we can patch out an overly-strict version bound; if/when this is
fixed upstream we can go back to using the hackage version. See:
<https://github.com/rcook/sexpr-parser/issues/6>
