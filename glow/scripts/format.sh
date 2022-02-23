#!/usr/bin/env sh

ormolu --mode inplace $(find ./exe ./lib ./tests -type f -name '*.hs')
