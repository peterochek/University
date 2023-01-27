#!/bin/bash

max_size=15200000

for _ in {1..30} 
do
    ./newmem.sh $max_size &
	sleep 1
done
