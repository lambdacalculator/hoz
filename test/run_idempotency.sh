#!/bin/bash
set -e

# Build first
cabal build

HOZ_BIN=$(cabal list-bin hoz)

# List of fast tests that DO NOT use `-q` because that would hang when waiting for input
TEST_FILES=(
    "test/test_syntax.oz"
    "test/test_data.oz"
    "test/test_exceptions.oz"
)

echo "Running Idempotency Checks..."

for file in "${TEST_FILES[@]}"; do
    filename="${file%.oz}"
    ozk_file="${filename}.ozk"
    tmp_ozk="${filename}_tmp.ozk"

    echo " - Checking ${file}..."
    
    # 1. Generate the initial .ozk output by running the original file
    $HOZ_BIN "${file}" > /dev/null

    # 2. Run hoz again on the generated .ozk file in kernel mode (-k)
    # This should parse the kernel syntax and output identical kernel syntax
    $HOZ_BIN -k "${ozk_file}" "${tmp_ozk}" > /dev/null

    # 3. Diff the two .ozk files
    if diff -u "${ozk_file}" "${tmp_ozk}"; then
        echo "   [SUCCESS] ${file} is idempotent."
    else
        echo "   [FAILURE] ${file} idempotency failed! Output differs."
        rm -f "${tmp_ozk}"
        exit 1
    fi

    # 4. Clean up the temp file
    rm -f "${tmp_ozk}"
done

echo ""
echo "All idempotency checks passed!"
