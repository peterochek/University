#!/bin/bash

car="$1"
if [ "$car" = "" ]
then
    echo "Enter valid car!"
    exit 0
fi


echo "Colors:"
grep -i "$car" cars.txt | awk '{print $2}' | sort | uniq
echo

echo "Minimal values: "
grep -i "$car" cars.txt | awk '{print $2, $3}' | sort -nk2,2 | sort -uk1,1