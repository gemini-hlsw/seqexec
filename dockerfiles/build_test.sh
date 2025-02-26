#!/bin/bash

# Set variables
IMAGE_NAME="registry.gitlab.com/nsf-noirlab/gemini/rtsw/user-tools/seqexec-tester/seqexec-dev"
OUTPUT_DIR="$(pwd)/output"

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Build Docker image
echo "Building Docker image..."
docker build -t "$IMAGE_NAME" -f Dockerfile.gn_test ..

# Extract and copy tar file to host
echo "Extracting and copying tar file to host..."
docker run --rm -v "$OUTPUT_DIR:/output" "$IMAGE_NAME" sh -c "cp /seqexec/app/seqexec-server-gn-test/target/universal/*.tgz /output/"

echo "Build complete. Tar file has been copied to $OUTPUT_DIR"
