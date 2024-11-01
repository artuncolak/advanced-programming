#!/bin/bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

TEST_CASES=$(cat ./test-cases.json)

PASSED=0
TOTAL=0

while read -r test_case; do
    ((TOTAL++))

    input=$(echo "$test_case" | jq -r '.input')
    expected=$(echo "$test_case" | jq -r '.expectedOutput')

    output=$(dotnet run <<<"$input")

    if [ "$output" = "$expected" ]; then
        ((PASSED++))
        echo -e "${GREEN}PASS${NC}: Input: $input, Expected: $expected, Got: $output"
    else
        echo -e "${RED}FAIL${NC}: Input: $input, Expected: $expected, Got: $output"
    fi
done < <(echo "$TEST_CASES" | jq -c '.[]')

# Print summary
echo "Tests passed: $PASSED/$TOTAL"

# Check if all tests passed
if [ "$PASSED" -eq "$TOTAL" ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
