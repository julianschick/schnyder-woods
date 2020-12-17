#!/bin/bash

if [ -z "$1" ] 
then
    echo "Please pass <n> as argument."
    exit
fi

mkdir -p data

sage ../sage/vertexcon.sage $1 >> data/vertexcon.txt
rm ../sage/vertexcon.sage.py
rm gurobi.log 2> /dev/null
