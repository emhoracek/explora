#!/bin/zsh

echo "Compiling test suite with HPC."
ghc -fhpc -itest-suite/ -itest-suite/librarySpec -itest-suite/executableSpec -iexecutable -ilibrary test-suite/Spec.hs

echo "Running test suite."
./test-suite/Spec

echo "\nHPC tests:"
hpc markup Spec.tix

echo "\nCopying HTML files into folder."
mv *.html markup

echo "\nRemoving tix file."
rm Spec.tix
