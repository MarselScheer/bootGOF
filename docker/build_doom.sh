#!/bin/bash

./build.sh
sudo docker build -t doom:4.0.0 -f Dockerfile.doom .
