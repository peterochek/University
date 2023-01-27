#!/bin/bash

test -e top.log || touch top.log
echo -n "" > top.log


while true
do
    # (batch for input) (sort mem) (output in mB) (delay 1 second) (top procs + useful info)
	top -b -o %MEM -E m -d 1 | head -12 | tail -9 >> top.log
	echo "" >> top.log
	sleep 1 
done