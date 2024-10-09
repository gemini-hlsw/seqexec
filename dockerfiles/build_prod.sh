#!/bin/bash

# Set variables
IMAGE_NAME="registry.gitlab.com/nsf-noirlab/gemini/rtsw/user-tools/seqexec-tester/seqexec-prod-7"
OUTPUT_DIR="$(pwd)/output"

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Build Docker image
echo "Building production Docker image..."
docker build -t "$IMAGE_NAME" -f Dockerfile.gn ..

# Extract and copy tar file to host
echo "Extracting and copying production tar file to host..."
docker run --rm -v "$OUTPUT_DIR:/output" "$IMAGE_NAME" sh -c "cp /seqexec/app/seqexec-server-gn/target/universal/*.tgz /output/"

echo "Production build complete. Tar file has been copied to $OUTPUT_DIR"
