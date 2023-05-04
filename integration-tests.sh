#!/bin/bash

# List of strings to iterate through
tests=("TopReactorEx")

make clean
for test in "${tests[@]}"
do
  make run TOP="$test"
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
echo "PASSING TESTS: $tests"
echo "-----------------------------------------------------------"



