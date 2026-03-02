#!/usr/bin/bash
cabal run TreeSitterAST -- --code-gen "./sample/grammar.json" --output-dir "./sample"
