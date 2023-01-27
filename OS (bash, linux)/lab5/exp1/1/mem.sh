#!/bin/bash

test -e report.log || touch report.log
echo -n "" > report.log

array=()
counter=0
size=0

while true
do
	array+=(1 2 3 4 5 6 7 8 9 10)
	counter=$(( counter + 1 ))
	size=$(( size + 10 ))
	if [[ "$counter" == 100000 ]]; then
		echo $size >> report.log
		counter=0
	fi
done
