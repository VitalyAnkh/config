#!/usr/bin/env bash
export SDK_DIR="${HOME}/sdk"
echo "********************************************************************************"
echo "update idris from github repo..."
cd "${SDK_DIR}/Idris-dev"
git pull
make
make relib
make test
echo "********************************************************************************"
echo "update idris2 from github repo..."
cd "${SDK_DIR}/Idris2"
git pull
make idris2
make install