#!/bin/bash

test -e report.log || touch report.log
echo -n "" > report.log

array=()
size=0

while true
do
	array+=(1 2 3 4 5 6 7 8 9 10)
	size=$(( size + 10 ))
	if [[ "$size" -ge "$1" ]]; then
		break
	fi
done
