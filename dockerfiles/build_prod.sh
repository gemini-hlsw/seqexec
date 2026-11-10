#!/bin/bash

# Stop on a failed docker build, so a stale tarball is not copied and reported as built
set -e

# Set variables
IMAGE_NAME="registry.gitlab.com/nsf-noirlab/gemini/rtsw/user-tools/seqexec-tester/seqexec-prod"
OUTPUT_DIR="$(pwd)/output"

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Build Docker image
echo "Building production Docker image..."
docker build -t "$IMAGE_NAME" -f Dockerfile.prod ..

# Extract and copy tar file to host
echo "Extracting and copying production tar file to host..."
docker run --rm -v "$OUTPUT_DIR:/output" "$IMAGE_NAME" sh -c "cp /seqexec/app/seqexec-server-gn/target/universal/*.tgz /output/"

echo "Production build complete. Tar file has been copied to $OUTPUT_DIR"
