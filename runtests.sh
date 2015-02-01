#!/bin/zsh

echo "Compiling test suite with HPC."
ghc -fhpc -itest-suite/ -itest-suite/librarySpec -itest-suite/executableSpec -iexecutable -ilibrary test-suite/Spec.hs

echo "Running test suite."
./test-suite/Spec

echo "\nHPC tests:"
hpc report Spec.tix

echo "\nRemoving tix file."
rm Spec.tix
