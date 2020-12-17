#!/bin/bash

mkdir -p data

rm data/diameter.txt 2> /dev/null
sage ../sage/diameter.sage > data/diameter.txt
rm ../sage/diameter.sage.py
