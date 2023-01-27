#!/bin/bash

test -e report2.log || touch report2.log
echo -n "" > report2.log

array=()
counter=0
size=0

while true
do
	array+=(1 2 3 4 5 6 7 8 9 10)
	counter=$(( counter + 1 ))
	size=$(( size + 10 ))
	if [[ "$counter" == 100000 ]]; then
		echo $size >> report2.log
		counter=0
	fi
done
