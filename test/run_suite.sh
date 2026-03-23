#!/bin/bash
set -e

# Build first
cabal build

HOZ_BIN=$(cabal list-bin hoz)

echo "Running Syntax Test..."
$HOZ_BIN test/test_syntax.oz

echo "Running Kernel Syntax Test..."
$HOZ_BIN -k test/test_syntax.ozk

echo -e "\nRunning Data Test..."
$HOZ_BIN test/test_data.oz

echo -e "\nRunning Concurrency Test..."
$HOZ_BIN -q 10 test/test_concurrency.oz

echo -e "\nRunning ByNeed Test..."
$HOZ_BIN -q 10 test/test_byneed.oz

echo -e "\nRunning Remainder Test..."
$HOZ_BIN -q 10 test/test_remainder.oz

echo -e "\nRunning Exceptions Test..."
$HOZ_BIN test/test_exceptions.oz

echo -e "\nRunning Thread Expression Regression Test..."
THREAD_OUT=$($HOZ_BIN test/test_thread_expr.oz)
echo "$THREAD_OUT"
echo "$THREAD_OUT" | grep -q "_1: 3"

echo -e "\nState Invariant Check..."
$HOZ_BIN --check test/test_data.oz

echo -e "\nAll Tests Passed!"
