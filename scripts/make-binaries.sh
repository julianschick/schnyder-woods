#!/bin/bash

echo "Cleaning..."
(cd .. && cargo clean)
echo "Compiling..."
(cd .. && cargo build --release)
echo "Done."
