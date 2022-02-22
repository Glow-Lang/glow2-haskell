WIP Experimental Haskell rewrite of parts of glow.

Note that the code under Glow.Gerbil relies on the `sexpr-parser`
package, which specifies an old version of `base` in its dependencies;
see: <https://github.com/rcook/sexpr-parser/issues/6>. If `cabal build`
flags an error, try:

1. Check out the source to sexpr-parser somewhere
   (<https://github.com/rcook/sexpr-parser>).
2. Edit `sexpr-parser`'s .cabal file, setting the upper bounds on base
   to `<5` (it appears in 3 places).
3. Add a file `cabal.project.local` to this directory, with contents:

```
packages:
  *.cabal
  /path/to/sexpr-parser/*.cabal
```

Then, `cabal build` should build your local checkout, solving the
versioning issue.
