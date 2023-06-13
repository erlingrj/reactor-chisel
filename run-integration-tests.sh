#!/bin/bash

# List of strings to iterate through
tests_codesign=("TopReactorEx")
tests_standalone=("ReactorCounter")

make clean
for test in "${tests_standalone[@]}"
do
  make run TOP="$test" PROGRAM_TYPE=standalone
  if [ $? -ne 0 ]; then
    echo "-----------------------------------------------------------"
    echo "Integration-test FAILED!"
    echo "FAILING TEST: $test"
    echo "-----------------------------------------------------------"
    exit 1
  fi
  make clean
done

for test in "${tests_codesign[@]}"
do
  make run TOP="$test" PROGRAM_TYPE=codesign
  if [ $? -ne 0 ]; then
    echo "-----------------------------------------------------------"
    echo "Integration-test FAILED!"
    echo "FAILING TEST: $test"
    echo "-----------------------------------------------------------"
    exit 1
  fi
  make clean
done

echo "-----------------------------------------------------------"
echo "Integration-test SUCCEEDED!"
echo "PASSING STANDALONE TESTS: ${tests_standalone[@]}"
echo "PASSING CODESIGN TESTS: ${tests_codesign[@]}"
echo "-----------------------------------------------------------"



