WIP Experimental Haskell rewrite of parts of glow.

# Building

We use plain old cabal to build glow (not stack). To build:

```sh
cabal build all
```

We currently develop using the GHC 8.10.x series; if your distribution
ships something different, we recommend using `ghcup` to get an
appropriate version of the compiler.

# Style guidelines

Everything in this section is a *guideline*, not a hard-and-fast rule;
don't deviate from this for no reason, but if there is a strong reason
to go against this advice, just do it, don't ask permission.

## Language Extensions

Using more advanced language features is OK, but make sure they're
actually buying you something that's worth the extra complexity &
cognitive load. Err on the side of keeping it simple.

## Formatting

Use `ormolu` to auto-format the code. `scripts/format.sh` will do
this for the whole codebase, but you should set up your editor to use
it.

Stick to ascii; no Greek letters in variable names, use `forall` instead
of `∀`, etc. This rule is mainly to make sure our style is easy to
actually type. The one exception to this is if you have a unicode
character in a string literal, in which case the literal character is
usually preferred.
