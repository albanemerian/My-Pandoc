#!/bin/bash

# Base directories
MY_OUT_DIR="./my-out"
REAL_OUT_DIR="./real-out"
XML_DIR="./xml"

# Initialize test result
ALL_TESTS_PASSED=true

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color (reset)

ghc Tester.hs -o docparser -package text -package bytestring -package aeson -package xml-conduit -package blaze-html -package cmark
# Loop through all files in the xml directory
for XML_FILE in "$XML_DIR"/*.xml; do
    # Get the filename
    FILENAME=$(basename "$XML_FILE" .xml)

    # Construct the corresponding output file paths
    MY_OUT_FILE="$MY_OUT_DIR/$FILENAME.txt"
    REAL_OUT_FILE="$REAL_OUT_DIR/$FILENAME.txt"

    # Run pandoc and docparser commands
    ./mypandoc -i "$XML_FILE" -f xml -e xml  -o temp.xml
    ./docparser xml temp.xml > "$MY_OUT_FILE"
    ./docparser xml "$XML_FILE" > "$REAL_OUT_FILE"

    # Normalize files for comparison
    NORMALIZED_MY=$(mktemp)
    NORMALIZED_REAL=$(mktemp)

    expand -t 4 "$MY_OUT_FILE" | \
    sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*$//g' -e '/^[[:space:]]*$/d' > "$NORMALIZED_MY"

    expand -t 4 "$REAL_OUT_FILE" | \
    sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*$//g' -e '/^[[:space:]]*$/d' > "$NORMALIZED_REAL"

    # Compare the normalized files
    if ! diff "$NORMALIZED_MY" "$NORMALIZED_REAL" > /dev/null; then
        echo -e "${RED}✗ Test failed: $FILENAME differs.${NC}"
        ALL_TESTS_PASSED=false
    else
        echo -e "${GREEN}✓ Test passed: $FILENAME${NC}"
    fi

    rm "$NORMALIZED_MY" "$NORMALIZED_REAL" temp.xml

done

rm Tester.hi Tester.o docparser
# Final result
if $ALL_TESTS_PASSED; then
    echo "All tests passed."
    exit 0
else
    echo "Some tests failed."
    exit 1
fi
