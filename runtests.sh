#!/bin/zsh

STR="Spec"

echo "${1+$STR}"

echo "Running tests: "
runhaskell -itest-suite -ilibrary test-suite/${1+$STR}.hs
