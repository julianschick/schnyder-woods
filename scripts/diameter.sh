#!/bin/bash

if [ -z "$1" ] 
then
    echo "Please pass <n> as argument."
    exit
fi

mkdir -p data

sage ../sage/diameter.sage $1 >> data/diameter.txt
rm ../sage/diameter.sage.py
